{- | This module is responsible for mouse input.

-}
module DeckPlayer.Input.Mouse where

import SDL

import DeckPlayer.TypesConstants
import Control.Monad (foldM)
import DeckFormat.DeckFormat
import Foreign.C (CInt)
import qualified Data.Set as Set
import Control.Lens

import DeckPlayer.Assets
import DeckFormat.Structure (DeckDirectory(..))

import DeckPlayer.Action

{- | Set the cursor to a specific cursor.

== Note(s)

This is maybe more of a render/draw specific thing, but it's here for now.

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

-- FIXME: move to an input module like mouse?
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

{- | Adjust a mouse coordinate based on the current display settings.

This is used to translate a mouse coordinate based on the current display settings. For
example, if full screen we need to adjust for the scale factor and the offset of the top
left.

-}
adjustedMouseCoord :: Renderer -> (CInt, CInt) -> Rectangle CInt -> IO (CInt, CInt)
adjustedMouseCoord renderer coord renderArea = do
    scaleFactor <- get $ rendererScale renderer
    -- now adjust the mouse coord based on the scale factor
    let
        (x, y) = coord
        (V2 scaleX scaleY) = scaleFactor
        (Rectangle (P (V2 offsetX offsetY)) _) = renderArea
    return
        ( round $ (fromIntegral x / scaleX) - (fromIntegral offsetX)
        , round $ (fromIntegral y / scaleY) - (fromIntegral offsetY)
        )

eventLoopDispatcher
    :: Renderer -> FilePath -> DeckState -> CardDeck -> [Event] -> Rectangle CInt -> IO DeckState
eventLoopDispatcher renderer deckPath deckState cardDeck events renderToArea = do
    -- FIXME: translate coordinates based on scale factor/full screen, etc.
    mouseClickEvents <- sequence
        [ adjustedMouseCoord renderer (fromIntegral x, fromIntegral y) renderToArea
        | e@(MouseButtonEvent (MouseButtonEventData _ Pressed _ _ _ (P (V2 x y)))) <-
            map eventPayload events
        ]
    mouseMotionEvents <- sequence
        [ adjustedMouseCoord renderer (fromIntegral x, fromIntegral y) renderToArea
        | e@(MouseMotionEvent (MouseMotionEventData _ _ _ (P (V2 x y)) _)) <-
            map eventPayload events
        ]
    let (_, currentCard) = deckState ^. deckStateCurrentCard

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