{- | Handle things relevant to rendering but not necessarily drawing.

-}
module DeckPlayer.Renderer (presentTarget, renderToggleFullscreen) where
import SDL (Renderer, Window, V2 (..), glGetDrawableSize, WindowMode (..), getWindowConfig, WindowConfig (..), setWindowMode, rendererScale, ($=), Rectangle(..), Texture, rendererRenderTarget, rendererDrawColor, V4 (V4), clear, get, windowSize, copy, present, Point (..))
import Control.Lens ((^.))
import qualified SDL.Video.Renderer as Renderer
import Foreign.C (CInt)

{- | Handle rendering the target texture to the screen. 

This function works within a system where everything gets drawn to a texture first, and
then that texture is drawn to the screen.

This includes centering the texture on the screen (such as when we are full screen,
handling the case of scale).

-}
presentTarget
    :: Renderer
    -> Window
    -> Texture
    -- ^ The texture to render to the screen.
    -> (Int, Int)
    -- ^ "Original" resolution of the target texture (before scaling).
    -> IO (Rectangle CInt)
    -- ^ The rectangle of the area on screen where the texture was rendered.
presentTarget renderer window targetTexture textureResolution@(textureWidth, textureHeight) = do
    -- Reset render target to the screen
    rendererRenderTarget renderer $= Nothing

    -- Clear the screen
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer

    -- Get the renderer's scale factor
    V2 scaleX scaleY <- get $ Renderer.rendererScale renderer

    -- Get the window size
    V2 windowWidth windowHeight <- get $ windowSize window

    -- Calculate the effective size of the texture after scaling using floating-point arithmetic
    let scaledTextureWidth = fromIntegral (fst textureResolution) * scaleX
        scaledTextureHeight = fromIntegral (snd textureResolution) * scaleY

    -- Calculate the position to center the scaled texture, then round to the nearest integer
    let centerPosX = round $  ((fromIntegral windowWidth - scaledTextureWidth) / 2) / scaleX
        centerPosY = round $ ((fromIntegral windowHeight - scaledTextureHeight) / 2) / scaleY
    let centerPos = V2 centerPosX centerPosY
    -- Render the texture to the screen
    -- potential here in the rectangles and positions used for cool transitions
    let renderToArea = Rectangle (P centerPos) (V2 (fromIntegral textureWidth) (fromIntegral textureHeight))
    copy renderer targetTexture Nothing (Just renderToArea)
    present renderer
    pure renderToArea

isFullscreen :: Window -> IO Bool
isFullscreen window = do
    windowConfig <- getWindowConfig window
    pure $ windowMode windowConfig == FullscreenDesktop

renderToggleFullscreen :: Renderer -> Window -> (Int, Int) -> IO ()
renderToggleFullscreen renderer window nativeResolution = do
    isFullscreen' <- isFullscreen window
    let
        newMode =
            if isFullscreen'
                then Windowed
                else FullscreenDesktop
    setWindowMode window newMode

    if newMode == FullscreenDesktop
        then do
            V2 screenWidth screenHeight <- glGetDrawableSize window
            let
                contentWidth = fromIntegral (fst nativeResolution)
                contentHeight = fromIntegral (snd nativeResolution)
                scaleFactor =
                    min (fromIntegral screenWidth / contentWidth) (fromIntegral screenHeight / contentHeight)
            rendererScale renderer $= V2 scaleFactor scaleFactor
        else rendererScale renderer $= V2 1 1