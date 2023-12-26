{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module DeckPlayer.Animated where

import Control.Lens (makeLenses, (^.))
import SDL (Texture, Renderer, createTextureFromSurface, freeSurface, copy, surfaceDimensions)
import qualified SDL.Image as Image
import qualified SDL
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import SDL.Vect (V2(..), Point(..))
import Foreign.C.Types (CInt)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist)
import Control.Lens (to)
import Data.Fixed (mod')

-- Load a texture from a file path and automatically determine width and height
loadTextureFromFile :: Renderer -> FilePath -> IO (Texture, V2 CInt)
loadTextureFromFile renderer filePath = do
    -- Load the image from the file
    surface <- Image.load filePath
    texture <- SDL.createTextureFromSurface renderer surface
    size <- surfaceDimensions surface
    SDL.freeSurface surface -- Free the surface as it's no longer needed
    return (texture, size)

data AnimationFrame = AnimationFrame
    { _texture :: Texture
    , _frameDuration :: Int
    }

data Animation = Animation
    { _textureFrames :: [AnimationFrame]
    }

data AnimationSpecSpriteSheetRect = AnimationSpecSpriteSheetRect
    { _x :: Int
    , _y :: Int
    , _width :: Int
    , _height :: Int
    } deriving (Show, Generic, FromJSON)

data AnimationSpecFrame = AnimationSpecFrame
    { _frameImageAsset :: FilePath
    , _duration :: Maybe Int
    , _spriteRect :: Maybe AnimationSpecSpriteSheetRect
    } deriving (Show, Generic, FromJSON)

data AnimationSpec = AnimationSpec
    { _defaultFrameRate :: Int
    , _frames :: [AnimationSpecFrame]
    } deriving (Show, Generic, FromJSON)

makeLenses ''Animation
makeLenses ''AnimationFrame
makeLenses ''AnimationSpec
makeLenses ''AnimationSpecFrame
makeLenses ''AnimationSpecSpriteSheetRect


type FancyTexture = (Texture, Animation)

{- | Converts an 'Animation' to a 'FancyTexture' by taking the first frame's texture.

Errors if the 'Animation' has no frames (because of `head`).

-}
animationToFancyTexture :: Animation -> FancyTexture
animationToFancyTexture animation = (head $ animation ^. textureFrames ^. to (map (^. texture)), animation)

{- | If the 'FancyTexture' is an animation, this function will update the texture in 'fst`
to the current frame's texture defined in 'Animation'.

Will loop around.

--}
updateFancyTextureFrame
    :: Float
    -- ^ The total time elapsed.
    -> FancyTexture
    -> FancyTexture
updateFancyTextureFrame totalTime (_, animation) = (currentFrame ^. texture, animation)
  where
    frames = animation ^. textureFrames
    animationDuration = sum $ map (^. frameDuration) frames
    timeInCycle = totalTime `mod'` fromIntegral animationDuration
    currentFrame = selectFrame timeInCycle frames

selectFrame :: Float -> [AnimationFrame] -> AnimationFrame
selectFrame _ [] = error "No frames available"  -- Handle this case appropriately
selectFrame timeInCycle (f:fs)
    | timeInCycle < fromIntegral (f ^. frameDuration) = f
    | otherwise = selectFrame (timeInCycle - fromIntegral (f ^. frameDuration)) fs

loadAnimationFrame :: Renderer -> FilePath -> Int -> AnimationSpecFrame -> IO AnimationFrame
loadAnimationFrame renderer tempDir defaultFrameRate frameSpec = do
    let
        framePath = tempDir </> _frameImageAsset frameSpec
        frameRate = fromMaybe defaultFrameRate (frameSpec ^. duration)
    case _spriteRect frameSpec of
        Just spriteRect -> loadTextureFromSpritesheet renderer framePath frameRate frameSpec spriteRect
        Nothing -> do
            (texture, _) <- loadTextureFromFile renderer framePath
            return $ AnimationFrame texture frameRate

loadTextureFromSpritesheet :: Renderer -> FilePath -> Int -> AnimationSpecFrame -> AnimationSpecSpriteSheetRect -> IO AnimationFrame
loadTextureFromSpritesheet renderer framePath frameRate frameSpec spriteRect = do
    -- Load the texture from the file path specified in the frame specification
    (texture, _) <- loadTextureFromFile renderer framePath

    -- Create a source rectangle based on the spriteRect
    let srcRect = SDL.Rectangle (P $ V2 (fromIntegral $ _x spriteRect) (fromIntegral $ _y spriteRect))
                                (V2 (fromIntegral $ _width spriteRect) (fromIntegral $ _height spriteRect))

    -- Create a new texture for the cropped image
    let textureAccess = SDL.TextureAccessTarget
    let cropSize = V2 (fromIntegral $ _width spriteRect) (fromIntegral $ _height spriteRect)
    croppedTexture <- SDL.createTexture renderer SDL.RGBA8888 textureAccess cropSize
    SDL.textureBlendMode croppedTexture SDL.$= SDL.BlendAlphaBlend

    -- Set the render target to the cropped texture
    SDL.rendererRenderTarget renderer SDL.$= Just croppedTexture
    SDL.clear renderer
    SDL.copy renderer texture (Just srcRect) (Just $ SDL.Rectangle (P $ V2 0 0) cropSize)

    -- Reset the render target to default
    SDL.rendererRenderTarget renderer SDL.$= Nothing

    -- Free the original texture
    SDL.destroyTexture texture

    return $ AnimationFrame croppedTexture frameRate

loadAnimationFrames :: Renderer -> FilePath -> AnimationSpec -> IO [AnimationFrame]
loadAnimationFrames renderer tempPath animSpec = forM (_frames animSpec) (loadAnimationFrame renderer tempPath (animSpec ^. defaultFrameRate))

createAnimationFromCustomFormat :: Renderer -> FilePath -> IO Animation
createAnimationFromCustomFormat renderer zipFilePath = do
    -- Get the system's temporary directory
    systemTempDir <- getTemporaryDirectory
    let tempDir = systemTempDir </> "animation_extract"

    -- Create a temporary directory for extraction
    createDirectoryIfMissing True tempDir

    -- Extract the zip archive
    archive <- Zip.toArchive <$> BSL.readFile zipFilePath
    Zip.extractFilesFromArchive [Zip.OptDestination tempDir] archive

    -- Read the animation spec JSON file from the temporary directory
    let specPath = tempDir </> "animation_spec.json"
    specExists <- doesFileExist specPath
    if specExists
        then do
            specContent <- BSL.readFile specPath
            case Aeson.eitherDecode specContent of
                Right animSpec -> do
                    frames <- loadAnimationFrames renderer tempDir animSpec
                    -- Clean up the temporary directory
                    removeDirectoryRecursive tempDir
                    return $ Animation frames
                Left err -> do
                    -- Clean up the temporary directory in case of an error
                    removeDirectoryRecursive tempDir
                    error $ "Failed to parse animation spec JSON: " ++ err
        else do
            -- Clean up the temporary directory if spec is not found
            removeDirectoryRecursive tempDir
            error "Animation spec JSON not found in the extracted directory"


