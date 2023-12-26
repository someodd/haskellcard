{-# LANGUAGE OverloadedStrings #-}
module Mock where

import Test.Hspec
import SDL
import Control.Lens ((^.), to)
import Control.Monad (unless)

import DeckPlayer.Animated
import Paths_haskellcard (getDataFileName)

shouldSatisfy_ :: HasCallStack => String -> a -> (a -> Bool) -> Expectation
shouldSatisfy_ errorMessage value predicate =
  unless (predicate value) $
    expectationFailure errorMessage

-- Define a sample zip file path for testing
sampleZipFilePath :: FilePath
sampleZipFilePath = "test/mocks/animation.zip"

-- Function to create a software renderer for testing
createTestRenderer :: IO (Renderer, Window)
createTestRenderer = do
    -- Initialize SDL
    initializeAll

    -- Create a window (even though we won't use it in software rendering)
    let windowConfig = defaultWindow { windowInitialSize = V2 800 600 }
    window <- createWindow "Test Window" windowConfig

    -- Create a software renderer
    renderer <- createSoftwareRenderer =<< getWindowSurface window

    return (renderer, window)

getMockAnimation :: IO Animation
getMockAnimation = do
    (renderer, window) <- createTestRenderer
    animationZipPath <- getDataFileName sampleZipFilePath
    animation <- createAnimationFromCustomFormat renderer animationZipPath
    cleanupTestRenderer renderer window
    return animation


-- Function to clean up and destroy the renderer and window
cleanupTestRenderer :: Renderer -> Window -> IO ()
cleanupTestRenderer renderer window = do
    destroyRenderer renderer
    destroyWindow window

mockHspec = do
    describe "createAnimationFromCustomFormat" $ do
        it "should load a three frame animation from a zip file" $ do
            animation <- getMockAnimation

            -- Check if the animation contains the expected number of frames
            length (_textureFrames animation) `shouldBe` 3

        it "should use the default frame rate when none is specified" $ do
            animation <- getMockAnimation

            let defaultFrameRate' = 100

            -- Test each frame's frame rate (where defaulted is expected)
            let firstFrameDuration = animation ^. textureFrames . to (!! 0) . frameDuration
            firstFrameDuration `shouldBe` defaultFrameRate'

            let thirdFrameDuration = animation ^. textureFrames . to (!! 2) . frameDuration
            thirdFrameDuration `shouldBe` defaultFrameRate'

        it "should correctly set the frame rate for frames with specified durations" $ do
            animation <- getMockAnimation
            let secondFrameDuration = animation ^. textureFrames . to (!! 1) . frameDuration
            secondFrameDuration `shouldBe` 500

    -- FIXME: test each frame!
    describe "updateFancyTextureFrame" $ do
        it "selects the correct frame based on totalTime" $ do
            (renderer, window) <- createTestRenderer
            zipFilePath <- getDataFileName sampleZipFilePath
            animation <- createAnimationFromCustomFormat renderer zipFilePath

            let
                pickFrame fancyTexture index = (snd $ fancyTexture) ^. textureFrames . to (!! index) . texture
                startingFancyTexture = (animation ^. textureFrames . to (!! 0) . texture, animation)

            -- Should be at frame 1 (at index 0) initially. Frame 1 is 100ms long.
            let initialFrameTexture = pickFrame startingFancyTexture 0
            shouldSatisfy_ "At frame 1 (index 0) initially. 0ms elapsed." (fst startingFancyTexture) (== initialFrameTexture)

            -- Should be at frame 1 (at index 0) after 50ms.
            let fancyTextureStillFrame1 = updateFancyTextureFrame 50 startingFancyTexture
            shouldSatisfy_ "Still at frame 1 (index 0) after 50ms" (fst startingFancyTexture) (== fst fancyTextureStillFrame1)

            -- Should be frame 2 (at index 1) after 150ms.
            let
                updatedFancyTexture = updateFancyTextureFrame 150 fancyTextureStillFrame1
                expectedFrameTexture = pickFrame updatedFancyTexture 1
                unexpectedFrameTexture = pickFrame updatedFancyTexture 0
                otherUnexpectedFrameTexture = pickFrame updatedFancyTexture 2
            shouldSatisfy_ "Frame 2 (index 1) after 150ms." (fst updatedFancyTexture) (== expectedFrameTexture)
            -- I added this check to ensure equality works as expected with textures.
            shouldSatisfy_ "Frame equality operation check" (fst updatedFancyTexture) (/= unexpectedFrameTexture)
            shouldSatisfy_ "Another frame equality operation check" (fst updatedFancyTexture) (/= otherUnexpectedFrameTexture)

            -- Should be at frame 3 (at index 2) after 601ms elapsing.
            let
                updatedFancyTexture2 = updateFancyTextureFrame 601 updatedFancyTexture
                expectedFrameTexture2 = pickFrame updatedFancyTexture2 2
            shouldSatisfy_ "Frame 3 (index 2) after 601ms" (fst updatedFancyTexture2) (== expectedFrameTexture2)

            cleanupTestRenderer renderer window

        -- FIXME: use animationToFancyTexture
        it "loops back to the first frame after completing the cycle" $ do
            animation <- getMockAnimation
            let fancyTexture = (animation ^. textureFrames . to (!! 0) . texture, animation)
            let updatedFancyTexture = updateFancyTextureFrame 650 fancyTexture
            let selectedFrameDuration = (snd $ updatedFancyTexture) ^. textureFrames . to (!! 0) . frameDuration
            selectedFrameDuration `shouldBe` 100