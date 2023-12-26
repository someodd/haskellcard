{- |
    System for managing resources assisting in the dynamic loading and unloading of said
    resources.

    Maybe you could consider this a garbage collection system for assets.

    = Overview

    The basics are that this system allows resources like music and graphics to be loaded
    into a 'HashMap'/memory enabling these resources to be looked up by its 'AssetPath'
    (the path to the asset relative to the deck's root directory), rather than the actual
    path on disk and loading from disk. Besides avoiding reloading from disk when not
    needed, once a resource is loaded it can just be looked up by its 'AssetPath'
    effectively in memory. Another benefit is that instead of worrying about ensuring
    resources are freed after they're done being used, the system can handle unloading
    everything that was loaded into the 'AssetRegistry' for us.

    The system is called the 'AssetRegistry'. The 'AssetRegistry' can, with simple
    functions here, dynamically load all assets found within a given context and unload
    all assets.

    = Caveat(s)

    There's some confusion about paths/keys in this module that should get ironed out
    soon.

    A current annoyance with this module is that the system of the 'AssetRegistry' keys
    doesn't seem to follow a consistent convention. Sometimes basenames are used,
    sometimes it's a relative path to the deck's root directory, sometimes it's just the
    filename, I think. I should maybe hash this out soon by calling assets referred to in
    JSON as 'AssetName' and the key to assets as simply 'AssetKey' with some kind of type
    maybe like (DeckRoot, AssetKey). This is maybe why 'AssetPath' is ambiguous.
-}
module DeckPlayer.Assets (
    -- * Types
    AssetRegistry (..),
    Asset (..),
    Image (..),
    Audio (..),

    -- * Functions for managing assets
    updateLoadAssetRegistry,
    appendAssetRegistry,
    assetLookup,
    assetLookupError,
    loadAssets,
    unloadAssets,
) where

import Control.Monad (forM_)
import Data.ByteString qualified as ByteString (ByteString, readFile)
import Data.HashMap.Strict qualified as HashMap -- FIXME: would lazy be better?
import Data.List (find, isSuffixOf)
import SDL (
    Renderer,
    Surface,
    Texture,
    destroyTexture,
    freeSurface,
 )
import SDL.Font qualified as Font
import SDL.Image as Img (load, loadTexture)
import SDL.Mixer qualified as Mixer

import DeckFormat.DeckFormat (Card, DeckMeta)
import DeckFormat.ExtractAssets (extractAssets)
import DeckFormat.Helpers (suffixToDirectoryMapping)
import DeckFormat.Structure (DeckDirectory (..), deckLookup)
import DeckPlayer.Animated
import Control.Lens ((^.))

{- | The main data type which represents the system for managing assets.

The 'AssetPath' is the card the assets belong to, as of now 'loadAssets' creates an
'AssetRegistry' based off all the 'Asset's found in a 'Card'.

The way keys are managed may change for consistency in the future. See 'AssetPath'.
-}
data AssetRegistry = AssetRegistry AssetPath (HashMap.HashMap AssetPath Asset)

appendAssetRegistry :: AssetRegistry -> [(AssetPath, Asset)] -> AssetRegistry
appendAssetRegistry (AssetRegistry assetPath assetRegistry) assets = AssetRegistry assetPath $ HashMap.union (HashMap.fromList assets) assetRegistry

-- | Texture wrapper to support animations and static images.
data Image = StaticImage Texture | AnimatedImage FancyTexture

{- | Audio wrapper to support SFX and Music.

For the `Music` constructor, the first `Bool` is whether or not the music should loop, the
second `Bool` is whether or not the music should interrupt the current song or wait for it
to finish.

-}
data Audio = Sfx Mixer.Chunk | Music Mixer.Music

{- | The 'Asset' type is a wrapper for all the different types of assets that can be
loaded into the 'AssetRegistry'.

    One of the uses is to help determine how an asset is loaded and unloaded.
-}
data Asset
    = AssetImage Image -- FIXME: rename to AssetTexture?
    | AssetAudio Audio
    | AssetScript ByteString.ByteString
    | AssetFont Font.Font
    | AssetSurface Surface -- NOTE: not sure I like this, kinda hacky! should just be DumbImage but for some reason cursor is this way.

-- | The key to an 'Asset' in the 'AssetRegistry'?
type AssetPath = FilePath

{- | Fetch an 'Asset' from the 'AssetRegistry'.

The 'AssetPath' will be determined by the 'DeckDirectory' and the 'FilePath' passed.
-}
assetLookup
    :: DeckDirectory
    -- ^ Basically defines the type of asset to look up, but will also inform where to look.
    -> FilePath
    -- ^ Path to the resource on disk, relative to the deck's root directory.
    -> AssetRegistry
    -- ^ The 'AssetRegistry' to look in.
    -> Maybe Asset
    -- ^ The 'Asset' if found.
assetLookup deckDirectory assetPath (AssetRegistry _ assetRegistry) = HashMap.lookup (deckLookup "" deckDirectory assetPath) assetRegistry

-- | Same as 'assetLookup' but throws an error if the 'Asset' is not found.
assetLookupError :: DeckDirectory -> FilePath -> AssetRegistry -> Asset
assetLookupError deckDirectory assetPath assetRegistry =
    case assetLookup deckDirectory assetPath assetRegistry of
        Just asset -> asset
        Nothing ->
            error
                $ "Asset ("
                ++ assetPath
                ++ ") not found: "
                ++ deckLookup "" deckDirectory assetPath
                ++ " here's the asset registry: "
                ++ (show . HashMap.keys)
                    (let (AssetRegistry _ assetRegistry') = assetRegistry in assetRegistry')

{- | Unload all assets in the registry, except for the assets defined to not unload.

= Note(s)

I think I'll change this to not unload assets from the card deck meta, or at least
have a boolean flag for it.

Maybe this should also return a new 'AssetRegistry' with all the unloaded assets
removed.

-}
unloadAssets :: AssetRegistry -> [FilePath] -> IO ()
unloadAssets (AssetRegistry _ assetRegistry) preserveTheseAssetKeys = do
    let assetsToUnload = HashMap.filterWithKey (\key _ -> key `notElem` preserveTheseAssetKeys) assetRegistry
    forM_ (HashMap.elems assetsToUnload) unloadAsset

{- | Free/destroy/unload a single asset.

This is primarily a helper function for 'unloadAssets'.

-}
unloadAsset :: Asset -> IO ()
unloadAsset asset = case asset of
    AssetImage (AnimatedImage (currentFrameTexture, animation)) -> do
        destroyTexture currentFrameTexture -- FIXME: redundant?
        forM_ (animation ^. textureFrames) $ \frame -> do
            destroyTexture $ frame ^. texture
    AssetImage (StaticImage texture) -> SDL.destroyTexture texture
    AssetAudio (Sfx sound) -> Mixer.free sound
    AssetAudio (Music sound) -> Mixer.free sound  -- FIXME: need to come up with smart way to free based on rules because music should actually persist rooms! could have a music meta in deckstate?
    AssetFont font -> Font.free font
    AssetSurface surface -> SDL.freeSurface surface
    AssetScript _ -> return () -- No unloading needed for scripts

{- | Load all of the assets for the provided 'Card'. Works recursively!

You may be interested in looking at `DeckFormat.ExtractAssets` if you want to know how
this works.

-}
loadAssets
    :: Renderer
    -> FilePath
    -- ^ The path to the deck's root directory.
    -> DeckMeta
    -- ^ The deck's meta data. This is used to load the default assets for the deck and
    -- combine it with the assets from the 'Card'.
    -> (AssetPath, Card)
    -- ^ The card to load assets for. The 'AssetPath' corresponds to the 'Card'.
    -> IO AssetRegistry
loadAssets renderer deckPath meta (assetPath, card) = do
    let
        (cardAssets :: [(String, FilePath)]) = extractAssets card -- (File name, File Path) I think
        -- now we combine the card's assets with the deck defaults from meta... this is actually bad that we're constantly reloading the default assets! FIXME could just have metaAssets load into state
        (metaAssets :: [(String, FilePath)]) = extractAssets meta
        allAssets = metaAssets ++ cardAssets
    assets <- mapM (loadAsset renderer deckPath) allAssets
    return $ AssetRegistry assetPath $ HashMap.fromList assets

-- | Update the 'AssetRegistry' with a new asset.
updateLoadAssetRegistry
    :: Renderer
    -> AssetRegistry
    -- ^ The 'AssetRegistry' to update.
    -> FilePath
    -- ^ The path to the deck's root directory.
    -> DeckDirectory
    -- ^ The 'DeckDirectory' which corresponds to the type of asset to load.
    -> AssetPath
    -- ^ The name of the asset file to load (just the filename or base name).
    -> IO ((AssetPath, Asset), AssetRegistry)
updateLoadAssetRegistry renderer (AssetRegistry card assetRegistry) deckPath deckDirectory assetPath = do
    assetEntry@(assetPath', asset) <- loadAsset' renderer deckPath deckDirectory assetPath
    return (assetEntry, AssetRegistry card $ HashMap.insert assetPath' asset assetRegistry)

{- | Helper function which takes a function that returns an 'IO' action and applies it to
the second element of a tuple.

>>> secondIO (\a -> pure a) ("foo", "bar") :: IO (String, String)
("foo","bar")
-}
secondIO :: (a -> IO b) -> (c, a) -> IO (c, b)
secondIO f (c, a) = do
    b <- f a
    return (c, b)

{- | Defines an interface for loading assets. This makes it easier to dynamically load assets.

Related: 'directoryToLoadFunctionMapping'.

-}
type AssetLoader = Renderer -> FilePath -> IO Asset

-- | Maps a 'DeckDirectory' to a function that loads an asset of that type.
directoryToLoadFunctionMapping :: [(DeckDirectory, AssetLoader)]
directoryToLoadFunctionMapping =
    [ (DirectoryBackgrounds, loadImageAsset)
    , (DirectorySfx, loadSfxAsset)
    , (DirectoryScripts, loadScriptAsset)
    , (DirectoryObjects, loadImageAsset)
    , (DirectoryMusic, loadMusicAsset)
    , (DirectoryFonts, loadFontAsset)
    , (DirectoryCursors, loadSurfaceAsset)
    ]

-- | Helper function for 'loadAsset'.
loadAsset' :: Renderer -> FilePath -> DeckDirectory -> FilePath -> IO (AssetPath, Asset)
loadAsset' renderer deckPath directory assetPath = do
    let
        function =
            case lookup directory directoryToLoadFunctionMapping of
                Just f -> f
                Nothing -> error $ "Directory not mapped to an asset loader: " ++ show directory
    secondIO (function renderer) $ twoAssetPaths deckPath directory assetPath

{- | Assists in loading an asset according to the suffix of the JSON field name it was
found in.

Used as a helper function for 'loadAssets'. Also check out 'suffixToDirectoryMapping'.

-}
loadAsset :: Renderer -> FilePath -> (String, FilePath) -> IO (AssetPath, Asset)
loadAsset renderer deckPath (assetName, assetPath) =
    case find ((`isSuffixOf` assetName) . fst) suffixToDirectoryMapping of
        Just (_, directory) -> loadAsset' renderer deckPath directory assetPath
        Nothing -> error $ "Unknown asset type: " ++ assetName

{- | Helper function for 'loadAsset''.

Returns the 'AssetPath' and the real path on disk, although currently
the real path is just the path relative to the deck's root directory.

-}
twoAssetPaths :: FilePath -> DeckDirectory -> FilePath -> (AssetPath, FilePath)
twoAssetPaths deckPath directory assetPath = (deckLookup "" directory assetPath, deckLookup deckPath directory assetPath)

loadSurfaceAsset :: AssetLoader
loadSurfaceAsset _ realPath = do
    surface <- Img.load realPath
    return $ AssetSurface surface

{- | Load a font.

Unfortunately the font size is hardcoded at the moment.

-}
loadFontAsset :: AssetLoader
loadFontAsset _ realPath = do
    font <- Font.load realPath 24
    return $ AssetFont font

loadImageAsset :: AssetLoader
loadImageAsset renderer realPath = do
    if ".zip" `isSuffixOf` realPath then do
        animation <- createAnimationFromCustomFormat renderer realPath >>= pure . animationToFancyTexture
        return $ AssetImage (AnimatedImage animation)
    else do
        texture <- Img.loadTexture renderer realPath
        return $ AssetImage (StaticImage texture)

loadSfxAsset :: AssetLoader
loadSfxAsset _ realPath = do
    sound <- Mixer.load realPath
    return $ AssetAudio $ Sfx sound

loadMusicAsset :: AssetLoader
loadMusicAsset _ realPath = do
    sound <- Mixer.load realPath
    return $ AssetAudio $ Music sound

loadScriptAsset :: AssetLoader
loadScriptAsset _ realPath = do
    scriptContent <- ByteString.readFile realPath
    return $ AssetScript scriptContent
