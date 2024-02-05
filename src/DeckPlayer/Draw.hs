-- FIXME: this module needs refactoring
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeckPlayer.Draw (drawCard, drawText) where

import qualified SDL.Font as Font
import System.FilePath ((</>))
import SDL
import Control.Monad (forM_)
import Control.Lens
 -- should i switch to lazy?

import DeckFormat.DeckFormat
import qualified Data.Text as T
import DeckPlayer.TypesConstants (filterOnAppearConditions, DeckState)
import DeckPlayer.TweenTransform
import DeckPlayer.Assets
import DeckFormat.Structure
import Control.Exception (catch)

handleSDLErrorTextureInfo :: Texture -> FilePath -> SDLException -> IO TextureInfo
handleSDLErrorTextureInfo texture path e = do
    error $ "Caught SDL exception (" ++ path ++ "): " ++ show e

drawObjects :: Renderer -> AssetRegistry -> FilePath -> [CardObject] -> IO ()
drawObjects renderer assetRegistry deckRoot cardObjects = do
    forM_ cardObjects $ \obj -> do
        ( (posX, posY), (scaledW, scaledH), objTexture ) <- applyTransformation assetRegistry obj
        let (posX', posY') = (fromIntegral posX, fromIntegral posY)
            (scaledW', scaledH') = (fromIntegral scaledW, fromIntegral scaledH)
        copy renderer objTexture Nothing (Just (Rectangle (P (V2 posX' posY')) (V2 scaledW' scaledH')))

drawBackgrounds :: Renderer -> AssetRegistry -> FilePath -> Card -> IO ()
drawBackgrounds render assetRegistry deckRoot card = do
    forM_ (card ^. cardBackgrounds) $ \bg -> do
        let
            bgTexture = case assetLookupError DirectoryBackgrounds (bg ^. cardBackgroundAsset) assetRegistry of
                (AssetImage (StaticImage bgTexture')) -> bgTexture'
                (AssetImage (AnimatedImage (bgTexture', _))) -> bgTexture'
                a -> error $ "Background asset is not an image: " ++ (bg ^. cardBackgroundAsset)
        textureInfo <- catch (queryTexture bgTexture) (handleSDLErrorTextureInfo bgTexture (bg ^. cardBackgroundAsset))
        let TextureInfo { textureWidth = bgW, textureHeight = bgH } = textureInfo
        copy render bgTexture Nothing (Just (Rectangle (P (V2 0 0)) (V2 bgW bgH)))

-- FIXME: maybe this is more of a wrapper called drawCardText
-- FIXME: it's stupid that this takes "TextDefaults"
-- FIXME: use assetLookup
drawText :: Renderer -> FilePath -> TextDefaults -> CardText -> IO ()
drawText renderer deckPath textDefaults cardText' = do
    -- Load font for drawing text
    let fontPathHelper fontFileName = deckPath </> "fonts" </> fontFileName
    font <- Font.load
        (maybe (fontPathHelper $ textDefaults ^. textDefaultsFontAsset) fontPathHelper (cardText' ^. textFontAsset))
        (maybe (textDefaults ^. textDefaultsSize) id (cardText' ^. textSize))
    let
        -- support rgba? TODO
        (r, g, b) = maybe (textDefaults ^. textDefaultsColor) id (cardText' ^. textColor)
        color = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
    textSurface <- Font.solid font color (T.pack $ cardText' ^. text)
    textTexture <- createTextureFromSurface renderer textSurface
    Font.free font

    -- Get texture dimensions for text
    TextureInfo {textureWidth = w, textureHeight = h} <- queryTexture textTexture

    -- Set where to draw the text
    let
        (x, y) = maybe (textDefaults ^. textDefaultsPosition) id (cardText' ^. textPosition)
        textPos = P $ V2 (fromIntegral x) (fromIntegral y)

    -- Draw the text
    copy renderer textTexture Nothing (Just (Rectangle textPos (V2 w h)))

-- COULD BE ABSTRACTED TO TAKE TEXT TO DRAW AND AT POSITION...
drawTitleText :: Renderer -> FilePath -> TextDefaults -> Card -> IO ()
drawTitleText renderer deckRoot textDefaults card = do
    -- Load font for drawing text
    font <- Font.load (deckRoot </> "fonts" </> textDefaults ^. textDefaultsFontAsset) 24  -- FIXME: use assetLookup!
    textSurface <- Font.solid font (V4 255 255 255 255) (T.pack $ card ^. cardName)
    textTexture <- createTextureFromSurface renderer textSurface
    Font.free font

    -- Get texture dimensions for text
    TextureInfo {textureWidth = w, textureHeight = h} <- queryTexture textTexture

    -- Set where to draw the text
    let textPos = P (V2 100 100)

    -- Draw the text
    copy renderer textTexture Nothing (Just (Rectangle textPos (V2 w h)))

drawCard :: Renderer -> AssetRegistry -> FilePath -> DeckState -> TextDefaults -> Card -> IO ()
drawCard renderer assetRegistry deckRoot deckState' textDefaults card = do
    -- Draw the card's background(s) first
    _ <- drawBackgrounds renderer assetRegistry deckRoot card

    -- FIXME: I don't like that the objects here are filtered on appear conditions <here>
    -- and other places too.
    _ <- maybe (pure ()) (drawObjects renderer assetRegistry deckRoot) (card ^. cardObjects >>= Just . filterOnAppearConditions deckState')

    _ <- drawTitleText renderer deckRoot textDefaults card
    maybe (pure ()) (drawText renderer deckRoot textDefaults) (card ^. cardText)
