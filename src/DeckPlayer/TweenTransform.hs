{- | Tweening animation.

A tween is two 'Transformation' states, with some sort of logic (like duration) which
decides how to get from one state to the other.

A tween will manipulate/produce a new 'Transformation'. This Tween module is simply
concerned with "tweens" and 'Transformation'. 'Transformation' is the state which a
tween manipulates.

== Note(s)

A bunch of functions may get refactored/moved around because I want this module to
only handle things relevant to tweening. That is, I don't want to use this module to
manipulate the state of a 'CardObject' or similar. Looser coupling is prefered, the
more generic the more potentially applicable the module will be to all kinds of
things.

-}
module DeckPlayer.TweenTransform (
    updateObjectsTween,
    applyTransformation,
) where

import Control.Lens ((&), (.~), (^.))
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import DeckFormat.DeckFormat
import SDL (TextureInfo (..), queryTexture)
import SDL.Video.Renderer (Texture)

import DeckFormat.Structure (DeckDirectory (DirectoryObjects))
import DeckPlayer.Assets (
    Asset (AssetImage),
    AssetRegistry,
    Image (..),
    assetLookup,
 )

-- FIXME?: there's a built in SDL.lerp
-- Linear interpolation
lerp :: Float -> Float -> Float -> Float
lerp a b t = a + (b - a) * t

-- Interpolate two (Int, Int) values
lerpTuple :: (Int, Int) -> (Int, Int) -> Float -> (Int, Int)
lerpTuple (x1, y1) (x2, y2) t =
    ( round $ lerp (fromIntegral x1) (fromIntegral x2) t
    , round $ lerp (fromIntegral y1) (fromIntegral y2) t
    )

-- Perform the tween
tween :: Tween -> Float -> Transformation
tween tween' progress =
    let
        t = if tween' ^. tweenOscillate
            then 0.5 * (1 - cos (2 * pi * progress)) -- Oscillation logic
            else (1 - cos (progress * pi)) / 2 -- Original smooth interpolation
    in
        Transformation
            { _transformPosition =
                lerpTuple
                    (tween' ^. tweenStart . transformPosition)
                    (tween' ^. tweenEnd . transformPosition)
                    t
            , _transformScale =
                lerp
                    (tween' ^. tweenStart . transformScale)
                    (tween' ^. tweenEnd . transformScale)
                    t
            , _transformRotate =
                lerp
                    (tween' ^. tweenStart . transformRotate)
                    (tween' ^. tweenEnd . transformRotate)
                    t
            }


{- | Calculate progress of the tween (how far into the tween, like are we 75% through the
animation?) based on tween's duration and the total time (ticks).

== Example(s)

>>> calculateProgress 3000 998514724742
21.845333
>>> calculateProgress 3000 3838838484899
87.38133
>>> calculateProgress 1000 6250
0.25
>>> calculateProgress 1000 6500
0.5
>>> calculateProgress 1000 6750
0.75
-}
calculateProgress :: Float -> Float -> Float
calculateProgress tweenDuration' elapsedTime = mod' elapsedTime tweenDuration' / tweenDuration'

{- | Main entrypoint for updating all of the 'CardObject's' 'Transform's based off of their
'Tween's.

-}
updateObjectsTween :: [CardObject] -> Float -> [CardObject]
updateObjectsTween objects elapsedTime = do
    map (updateObjectTween elapsedTime) objects

-- Update a single object's transformation based on tween progress
updateObjectTween :: Float -> CardObject -> CardObject
updateObjectTween elapsedTime object =
    case object ^. objectTween of
        Just tween' -> do
            let
                tweenDuration' = fromIntegral $ tween' ^. tweenDuration
                progress = calculateProgress tweenDuration' elapsedTime
            object & objectTransformation .~ Just (tween tween' progress)
        Nothing -> object

transformObjectPosition cardObject' (objW, objH) =
    -- Apply transformation if it exists
    let
        transform = fromMaybe (Transformation (0, 0) 1.0 0.0) $ cardObject' ^. objectTransformation
        (objX, objY) = cardObject' ^. objectPosition
        (transX, transY) = let (tX, tY) = transform ^. transformPosition in (objX + tX, objY + tY)
        scale = transform ^. transformScale
        -- Scale the object dimensions and position
        scaledW = round $ fromIntegral objW * scale
        scaledH = round $ fromIntegral objH * scale
        posX = fromIntegral transX
        posY = fromIntegral transY
    in
        ((posX, posY), (scaledW, scaledH))

-- FIXME: all it really needs is texture and position. can pass texture from animated. this is just a helper function that could go elsewhere too....
-- image (current frame), then it will be much less tightly coupled. can add a helper elsewhere...
{- | Return new position and dimensions based on applying the card's 'Transformation' onto
itself.

Performs a lookup to find the corresponding texture `Asset` associated with the
`CardObject`. The texture is used for determining the CardObject image size.
-}
applyTransformation
    :: AssetRegistry
    -> CardObject
    -> IO ((Int, Int), (Int, Int), Texture)
applyTransformation assetRegistry cardObject' = do
    let
        objectTexture = assetLookup DirectoryObjects (cardObject' ^. objectObjectAsset) assetRegistry
    case objectTexture of
        Just (AssetImage (StaticImage texture)) -> do
            textureInfo <- queryTexture texture
            let
                TextureInfo{textureWidth = objW, textureHeight = objH} = textureInfo
            pure $ let (x, y) = transformObjectPosition cardObject' (objW, objH) in (x, y, texture)
        Just (AssetImage (AnimatedImage (currentFrameTexture, _))) -> do
            textureInfo <- queryTexture currentFrameTexture
            let
                TextureInfo{textureWidth = objW, textureHeight = objH} = textureInfo
            pure $ let (x, y) = transformObjectPosition cardObject' (objW, objH) in (x, y, currentFrameTexture)
        Just _ ->
            error "Unsupported transformation of object asset type (only textures supported)."
        Nothing ->
            error $ "Object texture not found:" ++ cardObject' ^. objectObjectAsset
