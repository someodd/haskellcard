{- | "Play" a card deck.

This module ties everything together for actually playing a card deck!

-}

{-# LANGUAGE ScopedTypeVariables #-}
module DeckPlayer.DeckPlayer (playDeck) where

import Control.Lens
import Control.Monad (foldM, unless)
import Data.HashMap.Strict qualified as HashMap -- should i switch to lazy?
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import SDL
import SDL.Font qualified as Font
import SDL.Mixer qualified as Mix
import Data.Maybe (fromMaybe, isJust, fromJust)

import DeckFormat.DeckFormat
import DeckPlayer.Audio
import DeckPlayer.Draw
import DeckPlayer.Scripting
import DeckPlayer.TypesConstants
import DeckPlayer.TweenTransform
import DeckPlayer.Assets
import DeckFormat.Structure (DeckDirectory(..), titleCardName, deckLookup)
import DeckPlayer.Animated (updateFancyTextureFrame)

{- | Handle/perform an 'Action', possibly conditionally.

-}
handleAction :: Renderer -> Action -> CardDeck -> DeckState -> IO DeckState
handleAction renderer actionOrConditional deck deckState =
    case actionOrConditional of
        (ConditionalAction (Conditional conditionals actionOnTrue actionOnFalse)) -> do
            -- In the future there will be more conditionals and we'll basically do an AND match on them.
            let
                -- Maybe there's a better way to do this?
                -- need to do this as for condition in conditionals
                conditionalBoolMap = flip map conditionals $ \condition -> do
                    let conditionalFlag' = condition ^. conditionFlag
                    -- Apply these conditions if not Nothing
                    (conditionalFlag', performFlagConditional)
                conditionsToApply = map (\s -> snd s $ fromJust $ fst s) $ filter (isJust . fst) conditionalBoolMap
            if and conditionsToApply
                then do
                    case actionOnTrue of
                        Just actionOnTrue' ->
                            handleAction' actionOnTrue' deck deckState
                        Nothing ->
                            return deckState
                else do
                    case actionOnFalse of
                        Just actionOnFalse' -> handleAction' actionOnFalse' deck deckState
                        Nothing -> return deckState

        action -> handleAction' action deck deckState
   where
    performFlagConditional (ActionFlag requireThisFlag requireThisFlagValue) =
        let
            flagActualValue = requireThisFlag `Set.member` (deckState ^. deckStateFlags)
        in
            flagActualValue == requireThisFlagValue

    handleAction' :: Action -> CardDeck -> DeckState -> IO DeckState
    handleAction' action deck deckState =
        case action of
            (ActionTypeLink (ActionLink linkCard')) -> do
                let
                    newCard = HashMap.lookup linkCard' (deck ^. deckCards)
                case newCard of
                    Just card -> return $ deckState & deckStateCurrentCard .~ (linkCard', card)
                    Nothing -> return deckState
            (ActionTypeSfx (ActionSfx sfx)) -> do 
                let (AssetAudio (Sfx sfxChunk)) = assetLookupError DirectorySfx sfx (deckState ^. deckStateAssetRegistry)
                playSound sfxChunk
                return deckState
            (ActionTypeScript script) -> do
                let (AssetScript scriptAsset) = assetLookupError DirectoryScripts (script ^. actionScriptAsset) (deckState ^. deckStateAssetRegistry)
                runLuaScript renderer deck deckState scriptAsset
            (ActionTypeFlag (ActionFlag flagName' flagValue')) -> do
                return $ deckStateFlags .~ setFlag (deckState ^. deckStateFlags) flagName' flagValue' $ deckState
            conditionalAction@(ConditionalAction _) ->
                handleAction renderer conditionalAction deck deckState

{- | Checks if the cordinate '(Cint, Cint)' is inside of 'CardObject'.
    
This accounts for 'Transformation'.

-}
isInsideObject :: Renderer -> AssetRegistry -> FilePath -> (CInt, CInt) -> CardObject -> IO Bool
isInsideObject _ assetRegistry _ (x, y) obj = do
    ((objX, objY), (objW, objH), _) <- transformBasedOnTexture assetRegistry obj
    let (objX', objY') = (fromIntegral objX, fromIntegral objY)
        (objW', objH') = (fromIntegral objW, fromIntegral objH)
    pure $ x >= objX' && x <= objX' + objW' && y >= objY' && y <= objY' + objH'

hookMouseClickObject :: MouseEventProcessor
hookMouseClickObject renderer deckState cardDeck deckPath' (x, y) obj = do
    insideObject <- isInsideObject renderer (deckState ^. deckStateAssetRegistry) deckPath' (x, y) obj
    if insideObject
        then do
            -- Process onclick actions if any
            deckState' <- case obj ^. objectOnClick of
                [] -> return deckState
                actions -> foldM (\state action -> handleAction renderer action cardDeck state) deckState actions
            pure $ deckStateHoveredObjects
                .~ updateHoverStatus (deckState' ^. deckStateHoveredObjects) obj $ deckState'
        else do
            return $ deckStateHoveredObjects
                .~ Set.delete (show obj) (deckState ^. deckStateHoveredObjects) $ deckState

hookMouseHoverObject :: MouseEventProcessor
hookMouseHoverObject renderer deckState cardDeck deckPath' (x, y) obj = do
    insideObject <- isInsideObject renderer (deckState ^. deckStateAssetRegistry) deckPath' (x, y) obj
    if insideObject
        then do
            -- We are inside the object bounds, but does the object even have any onHover actions?
            case obj ^. objectOnHover of
                [] -> return deckState
                actions -> do
                    if inHoveredObjects obj (deckState ^. deckStateHoveredObjects)
                        then do
                            return deckState
                        else do
                            _ <- setCursor' (deckState ^. deckStateAssetRegistry) (cardDeck ^. deckMeta . metaCursorDefaults . cursorDefaultsHover)
                            let
                                deckState' =
                                    deckStateHoveredObjects
                                        .~ updateHoverStatus (deckState ^. deckStateHoveredObjects) obj $ deckState
                            foldM (\state action -> handleAction renderer action cardDeck state) deckState' actions
        else do
            -- The mouse is not inside the object bounds, so we know we can remove it from the set of hovered objects.
            _ <-
                setCursor' (deckState ^. deckStateAssetRegistry) (cardDeck ^. deckMeta . metaCursorDefaults . cursorDefaultsNormal)
            return $ deckStateHoveredObjects
                .~ Set.delete (show obj) (deckState ^. deckStateHoveredObjects) $ deckState

{- | Functions that handle mouse events should have this type signature.

In the future this may be expanded from simply (CInt, CInt) to a custom MouseEventData
type (so we can handle clicks, motions, wheel).
-}
type MouseEventProcessor =
    Renderer
    -> DeckState
    -> CardDeck
    -> FilePath
    -> (CInt, CInt)
    -> CardObject
    -> IO DeckState

{- | Send all the passed mouse events (coords right now) to the appropriate
'MouseEventProcessor', along with every object.

In other words, for each coord/mouse event we use a `MouseEventProcessor` on each object.
This allows us to accumulate/process each mouse event on each object. Note that sometimes
an event passed may not be within the bounds of any object (this logic is deferred to the
respective 'MouseEventProcessor').

-}
processMouseEvents
    :: MouseEventProcessor
    -> Renderer
    -> CardDeck
    -> FilePath
    -> DeckState
    -> [CardObject]
    -> [(CInt, CInt)]
    -> IO DeckState
processMouseEvents hookFunction renderer cardDeck deckPath initialDeckState objects coords =
    foldM (processCoord objects) initialDeckState coords
  where
    processCoord :: [CardObject] -> DeckState -> (CInt, CInt) -> IO DeckState
    processCoord objs currentState coord =
        foldM
            (\currentState' obj -> hookFunction renderer currentState' cardDeck deckPath coord obj)
            currentState
            objs

eventLoopDispatcher
    :: Renderer -> FilePath -> DeckState -> CardDeck -> [Event] -> IO DeckState
eventLoopDispatcher renderer deckPath deckState cardDeck events = do
    let
        mouseClickEvents =
            [ (fromIntegral x, fromIntegral y)
            | e@(MouseButtonEvent (MouseButtonEventData _ Pressed _ _ _ (P (V2 x y)))) <-
                map eventPayload events
            ]
        mouseMotionEvents =
            [ (fromIntegral x, fromIntegral y)
            | e@(MouseMotionEvent (MouseMotionEventData _ _ _ (P (V2 x y)) _)) <-
                map eventPayload events
            ]
        (_, currentCard) = deckState ^. deckStateCurrentCard

    case currentCard ^. cardObjects of
        Nothing -> return deckState
        Just objects -> do
            let objects' = filterOnAppearConditions deckState objects
            newDeckState <-
                processMouseEvents
                    hookMouseClickObject
                    renderer
                    cardDeck
                    deckPath
                    deckState
                    objects'
                    mouseClickEvents
            processMouseEvents
                hookMouseHoverObject
                renderer
                cardDeck
                deckPath
                newDeckState
                objects'
                mouseMotionEvents

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

    let initialCard = deck ^. deckTitleCard
    assetRegistry <- loadAssets renderer (deck ^. deckPath) (deck ^. deckMeta) (titleCardName, initialCard)
    let
        deckState = DeckState (titleCardName, initialCard) Set.empty Set.empty assetRegistry

    _ <- setCursor'
        (deckState ^. deckStateAssetRegistry)
        (deck ^. deckMeta . metaCursorDefaults . cursorDefaultsNormal)

    -- FIXME: update queue based on title card!
    -- could abstract this!
    musicQueue <- initializeMusicQueueVar initialCard (deckState ^. deckStateAssetRegistry)
    --Mix.whenMusicFinished $ musicFinishedCallback musicQueueVar

    appLoop renderer musicQueue deck deckState 0 window

    Font.quit -- Quit SDL2-ttf
    destroyRenderer renderer
    destroyWindow window
    quit

-- FIXME: does not need IO + should be renamed and redone a bit.
initializeMusicQueueVar :: Card -> AssetRegistry -> IO MusicQueue
initializeMusicQueueVar initialCard assetRegistry = do
    case initialCard ^. cardMusic of
        Nothing -> do
            pure $ initialMusicQueue
        Just cardMusic -> do
            pure $ MusicQueue { _musicQueueCurrentSong = Nothing, _musicFinishedSignal = True, _musicQueueNextSong = Nothing }

{- | Set the cursor to a specific cursor.

== Note(s)

This may be moved to another module in the future.

-}
setCursor' :: AssetRegistry -> CursorSpec -> IO Cursor
setCursor' assetRegistry cursorSpec = do
    -- FIXME: this should always evaluate to AssetSurface so it should have some kind of instance or something to restrain it to such or something
    -- i recall there's some way to help with type inference like if provided DirectoryCursors we'll know we're giving an AssetSurface. could also make
    -- it error if not expected type
    let (AssetSurface surface) = assetLookupError DirectoryCursors (cursorSpec ^. cursorSpecCursorAsset) assetRegistry
    -- Load the image as a surface
    --surface <- load $ deckPath </> "cursors" </> cursorSpec ^. cursorSpecImage

    -- Define the hotspot (the point of the cursor that represents the "click" location)
    -- Create a cursor from the surface
    let
        (x, y) = cursorSpec ^. cursorSpecHotspot
        hotspot = P $ V2 (fromIntegral x) (fromIntegral y)
    cursor <- createColorCursor surface hotspot

    -- Set the cursor
    activeCursor $= cursor

    pure cursor

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
currentCardChanged :: DeckState -> Bool
currentCardChanged deckState = fst (deckState ^. deckStateCurrentCard) /= (let (AssetRegistry cardAssetPath _) = deckState ^. deckStateAssetRegistry in cardAssetPath)

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

{- | The main loop of the application.

This is where the main loop of the application is defined. It is responsible for
updating the state, drawing the current card, and handling events.
-}
appLoop :: Renderer -> MusicQueue -> CardDeck -> DeckState -> Word32 -> Window -> IO ()
appLoop renderer musicQueue deck deckState lastTime window = do
    currentTime <- ticks -- I think delta time can be caculated with: fromIntegral (currentTime - lastTime) / 1000.0

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

    -- Update all animations in the registry
    -- helper function shuld amke i think (abstract out)
    let
        newRegistryAnimateUpdate = updateAnimations (deckStateRegistryChecked ^. deckStateAssetRegistry) (fromIntegral currentTime :: Float)
        deckStateAnimationsUpdate = deckStateRegistryChecked & deckStateAssetRegistry .~ newRegistryAnimateUpdate

    -- Update tweens/update the deckState's current card's objects
    let deckStateTweenUpdate = updateDeckStateTweens deckStateAnimationsUpdate currentTime

    -- Draw current card
    _ <- drawCard
        renderer
        (deckStateTweenUpdate ^. deckStateAssetRegistry)
        (deck ^. deckPath)
        deckStateTweenUpdate
        (deck ^. deckMeta . metaTextDefaults)
        (snd $ deckStateTweenUpdate ^. deckStateCurrentCard)

    -- Update screen
    present renderer

    -- Handle events (including checking for quit)
    events <- pollEvents
    let quitEvent = elem QuitEvent $ map eventPayload events
    deckState' <- eventLoopDispatcher renderer (deck ^. deckPath) deckStateTweenUpdate deck events

    unless quitEvent $ appLoop renderer newMusicQueue deck deckState' currentTime window
