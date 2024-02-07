{- | "Play" a card deck.

This module is the entrypoint for "playing" the card deck (besides 'Main').

The main game loop, initialization, and more.

-}

{-# LANGUAGE ScopedTypeVariables #-}
module DeckPlayer.DeckPlayer (playDeck) where

import Control.Lens
import Control.Monad (unless)
import Data.HashMap.Strict qualified as HashMap -- should i switch to lazy?
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word32)
import SDL hiding (Windowed)
import SDL.Font qualified as Font
import SDL.Mixer qualified as Mix
import Data.Maybe (fromMaybe)

import DeckFormat.DeckFormat
import DeckPlayer.Input.KeyMap (emptyKeyTimes, performAllKeyStateMapActionFunctions, KeyTimes)
import DeckPlayer.Audio
import DeckPlayer.Draw
import DeckPlayer.TypesConstants
import DeckPlayer.TweenTransform
import DeckPlayer.Assets
import DeckFormat.Structure (DeckDirectory(..), titleCardName, deckLookup)
import DeckPlayer.Animated (updateFancyTextureFrame)
import DeckPlayer.Renderer
import DeckPlayer.Input.KeyMappings (keyMappings)
import DeckPlayer.Input.Mouse ( eventLoopDispatcher, setCursor' )
import qualified Data.Aeson as Aeson
import DeckPlayer.Action (handleActions)


{- | Play a card deck.

A primary entrypoint.

This is the main entry point for playing a card deck. It initializes SDL2, SDL2_mixer,
and SDL2_ttf, and then starts the main loop.
-}
playDeck :: CardDeck -> IO ()
playDeck deck = do
    initializeAll
    let
        audio = Mix.defaultAudio 
            { -- A sampling frequency.
                Mix.audioFrequency = 48000
                --Mix.audioFormat = Mix.FormatU16_MSB
            }
    Mix.openAudio audio 8192 -- Initialize SDL-mixer
    Font.initialize -- Initialize SDL2-ttf
    let
        windowConfig =
            defaultWindow
                { windowInitialSize =
                    V2
                        (fromIntegral $ fst $ deck ^. deckMeta . metaResolution)
                        (fromIntegral $ snd $ deck ^. deckMeta . metaResolution)
                , windowResizable = False
                }
    window <- createWindow (T.pack $ deck ^. deckMeta . metaName) windowConfig
    renderer <- createRenderer window (-1) defaultRenderer

    (deckState, initialCard) <- initializeDeckState renderer deck

    _ <- setCursor'
        (deckState ^. deckStateAssetRegistry)
        (deck ^. deckMeta . metaCursorDefaults . cursorDefaultsNormal)

    let (nativeWidth, nativeHeight) = deckState ^. deckStateDisplaySettings . displaySettingsNativeResolution
    targetTexture <- offScreenRenderTarget renderer (fromIntegral nativeWidth, fromIntegral nativeHeight)

    musicQueue <- initializeMusicQueueVar initialCard (deckState ^. deckStateAssetRegistry)

    appLoop renderer musicQueue deck deckState 0 window emptyKeyTimes targetTexture

    -- Clean up, free resources, quit
    Font.quit
    destroyTexture targetTexture
    destroyRenderer renderer
    destroyWindow window
    quit

{- | Returns the initial 'DeckState' and the initial card.

-}
initializeDeckState :: Renderer -> CardDeck -> IO (DeckState, Card)
initializeDeckState renderer deck = do
    let initialCard = deck ^. deckTitleCard
    assetRegistry <- loadAssets renderer (deck ^. deckPath) (deck ^. deckMeta) (titleCardName, initialCard)
    let
        nativeResolution = deck ^. deckMeta . metaResolution
        displaySettings = DisplaySettings
            { _displaySettingsScaleType = Windowed nativeResolution
            , _displaySettingsNativeResolution = nativeResolution
            }
        deckState = DeckState (titleCardName, initialCard) Set.empty Set.empty assetRegistry displaySettings Aeson.Null
    return (deckState, initialCard)

-- FIXME: does not need IO + should be renamed and redone a bit.
initializeMusicQueueVar :: Card -> AssetRegistry -> IO MusicQueue
initializeMusicQueueVar initialCard assetRegistry = do
    case initialCard ^. cardMusic of
        Nothing -> do
            pure $ initialMusicQueue
        Just cardMusic -> do
            pure $ MusicQueue { _musicQueueCurrentSong = Nothing, _musicFinishedSignal = True, _musicQueueNextSong = Nothing }

{- | Update the 'AssetRegistry' if the current card has changed.

The change is detected by seeing if the 'DeckState' current card differs from the card
assigned to the 'AssetRegistry'.

If there is a change in the current card, the 'AssetRegistry' assets are unloaded, and
then reloaded with the new card's assets.

You can preserve certain assets to prevent them from being unloaded.
-}
updateRegistryIfCurrentCardDifferent
    :: Renderer
    -> [(DeckDirectory, FilePath)]
    -- ^ Do not unload and in fact preserve these assets in the new 'AssetRegistry'. Key.
    -> CardDeck
    -> DeckState
    -> IO DeckState
updateRegistryIfCurrentCardDifferent renderer preserveTheseAssetsKeys deck deckState = do
    -- First get the key/asset path of the current card
    let currentAssetRegistry = deckState ^. deckStateAssetRegistry
        assetsToPreserveMapping =
            map (\(dir, path) -> 
                    let asset = assetLookupError dir path currentAssetRegistry
                    in (deckLookup "" dir path, asset)) 
                preserveTheseAssetsKeys

    if currentCardChanged deckState
        then do
            _ <- unloadAssets (deckState ^. deckStateAssetRegistry) (map fst assetsToPreserveMapping)-- FIXME: use preserveTheseAssetsKeys here
            newAssetRegistry <- loadAssets renderer (deck ^. deckPath) (deck ^. deckMeta) (deckState ^. deckStateCurrentCard)
            let newDeckState' = deckState & deckStateAssetRegistry .~ appendAssetRegistry newAssetRegistry assetsToPreserveMapping
            pure newDeckState'
        else do
            return deckState

-- | Checks if the current card has changed.
--
-- Does not detect if it's the initial card first iteration.
currentCardChanged :: DeckState -> Bool
currentCardChanged deckState = fst (deckState ^. deckStateCurrentCard) /= (let (AssetRegistry cardAssetPath _) = deckState ^. deckStateAssetRegistry in cardAssetPath)

-- FIXME: how about move this to a musicqueue module?
-- FIXME: does not need IO anymore, rename
-- | Get the asset keys belonging to the current song and next song.
musicQueueVarAssetKeys :: MusicQueue -> IO [(DeckDirectory, FilePath)]
musicQueueVarAssetKeys musicQueue = do
    let
        currentSongAssetKey = case musicQueue ^. musicQueueCurrentSong of
            Nothing -> []
            Just currentSong -> [(DirectoryMusic, currentSong ^. musicMusicAsset)]
        nextSongAssetKey = case musicQueue ^. musicQueueNextSong of
            Nothing -> []
            Just nextSong -> [(DirectoryMusic, nextSong ^. musicMusicAsset)]
    return $ currentSongAssetKey ++ nextSongAssetKey

{- | Update the 'CardObject's' tweens in the 'DeckState'.

-}
updateDeckStateTweens :: Integral p => DeckState -> p -> DeckState
updateDeckStateTweens deckState currentTime =
    let currentObjects = fromMaybe [] $ snd (deckState ^. deckStateCurrentCard) ^. cardObjects
        updatedObjects = updateObjectsTween currentObjects (fromIntegral currentTime :: Float)
    in deckState & deckStateCurrentCard . _2 . cardObjects .~ Just updatedObjects

{- | Update all animations in the AssetRegistry so the texture is that of the current
frame.

-}
updateAnimations :: AssetRegistry -> Float -> AssetRegistry
updateAnimations (AssetRegistry belongsToPath assetRegistry) totalTime =
    AssetRegistry belongsToPath $ HashMap.map (modifyAssetRegistry totalTime) assetRegistry

-- | Function to update an animation in the asset registry if the entry is indeed an animation.
modifyAssetRegistry :: Float -> Asset -> Asset
modifyAssetRegistry totalTime (AssetImage (AnimatedImage fancyTexture@(_, animation))) =
    AssetImage $ AnimatedImage $ updateFancyTextureFrame totalTime fancyTexture
modifyAssetRegistry _ asset = asset

{- | If the current card is different from the last iteration and the current card has
onload actions, then perform the onload actions. But also, if it's the first iteration
in general/the first time the card is shown.

-}
performOnloadActions :: Renderer -> CardDeck -> Word32 -> DeckState -> IO DeckState
performOnloadActions renderer deck currentTicks deckState = do
    let
        currentCard = snd $ deckState ^. deckStateCurrentCard
        onloadActions = fromMaybe [] $ currentCard ^. cardOnLoad
    -- if it's the first iteration of a card change or its the first iteration of the first card...
    -- FIXME: extract to isnewcard?
    if currentCardChanged deckState || not (currentCardChanged deckState) && (snd (deckState ^. deckStateCurrentCard) == (deck ^. deckTitleCard))
        then
            handleActions renderer onloadActions deck currentTicks deckState
        else do
            pure deckState

{- | Perform some action(s) every iteration.

-}
performEachLoopActions :: Renderer -> CardDeck -> Word32 -> DeckState -> IO DeckState
performEachLoopActions renderer deck currentTicks deckState = do
    let
        currentCard = snd $ deckState ^. deckStateCurrentCard
        onTickActions = fromMaybe [] $ currentCard ^. cardEachLoop
    _ <- handleActions renderer onTickActions deck currentTicks deckState
    pure deckState

{- | The main loop of the application.

This is where the main loop of the application is defined. It is responsible for
updating the state, drawing the current card, and handling events.

-}
appLoop :: Renderer -> MusicQueue -> CardDeck -> DeckState -> Word32 -> Window -> KeyTimes -> Texture -> IO ()
appLoop renderer musicQueue deck deckState lastTime window keyTimes targetTexture = do
    currentTime <- ticks -- I think delta time can be caculated with: fromIntegral (currentTime - lastTime) / 1000.0

    -- Set the texture as the render target
    rendererRenderTarget renderer $= Just targetTexture

    -- Update the registry/do "garbage collection" for assets. We get music queue asset
    -- keys here because we want to preserve them from being unloaded by the "garbage
    -- collector."
    musicQueueAssetKeys <- musicQueueVarAssetKeys musicQueue
    deckStateRegistryChecked <- updateRegistryIfCurrentCardDifferent renderer musicQueueAssetKeys deck deckState

    -- Music queue logic
    newMusicQueue <- queueCheck musicQueue (deckStateRegistryChecked ^. deckStateAssetRegistry) (deckStateRegistryChecked ^. deckStateCurrentCard ^. _2 . cardMusic)
    
    -- Clear screen
    rendererDrawColor renderer $= defaultBackgroundColor
    clear renderer

    -- will perform the onload actions if the current card has changed
    -- FIXME: needs to get new state
    deckStateAfterOnloadActions <- performOnloadActions renderer deck currentTime deckStateRegistryChecked

    -- Update all animations in the registry
    -- helper function shuld amke i think (abstract out)
    let
        newRegistryAnimateUpdate = updateAnimations (deckStateAfterOnloadActions ^. deckStateAssetRegistry) (fromIntegral currentTime :: Float)
        deckStateAnimationsUpdate = deckStateAfterOnloadActions & deckStateAssetRegistry .~ newRegistryAnimateUpdate

    -- Update tweens/update the deckState's current card's objects
    let deckStateTweenUpdate = updateDeckStateTweens deckStateAnimationsUpdate currentTime

    -- FIXME: pass targetTExture
    -- Draw current card
    _ <- drawCard
        renderer
        (deckStateTweenUpdate ^. deckStateAssetRegistry)
        (deck ^. deckPath)
        deckStateTweenUpdate
        (deck ^. deckMeta . metaTextDefaults)
        (snd $ deckStateTweenUpdate ^. deckStateCurrentCard)



    -- perform everyloop actions. placed here so stuff gets rendered over the card stuff already rendered.
    deckStateAfterLoopActions <- performEachLoopActions renderer deck currentTime deckStateTweenUpdate

    -- Update screen, draw the targetTexture.
    renderToArea <- presentTarget renderer window targetTexture (deck ^. deckMeta . metaResolution)

    -- come before update screen?
    -- Handle events (including checking for quit)
    events <- pollEvents
    let quitEvent = elem QuitEvent $ map eventPayload events
    deckState' <- eventLoopDispatcher renderer currentTime (deck ^. deckPath) deckStateAfterLoopActions deck events renderToArea
    -- handle key input (mapping abstraction)
    keyFunc <- SDL.getKeyboardState
    (finalDeckState, finalKeyTimes) <- performAllKeyStateMapActionFunctions renderer window keyFunc currentTime (fromIntegral (currentTime - lastTime) / 1000.0) keyTimes keyMappings deckState'

    unless quitEvent $ appLoop renderer newMusicQueue deck finalDeckState currentTime window finalKeyTimes targetTexture
