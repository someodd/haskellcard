{- | The main types and constants where everything coalesces to represent the DeckPlayer,
basically.

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module DeckPlayer.TypesConstants where

import Data.Word (Word8)
import Control.Lens
import SDL hiding (Windowed)
 -- should i switch to lazy?
import qualified Data.Set as Set

import DeckFormat.DeckFormat
import DeckPlayer.Assets
import DeckPlayer.Input.KeyMap

{- | A set of hovered objects is kept track of to prevent re-activating hover actions
before the mouse has even left and re-entered.

This takes addresses the specific problem of an action re-triggering just because the
mouse keeps moving around inside the same object.

-}
type HoveredObjects = Set.Set String

-- FIXME: bad documentation
{- | Flags can be used for all kinds of things.  You can use them to only do certain
things if a flag is set.
    
-}
type Flags = Set.Set String

-- | Is the display fullscreen or windowed? If windowed, what is the resolution?
data ScaleType = FullScreen | Windowed (Int, Int)

{- | Important information about how to display the deck.

Contains general settings for rendering.

== Note(s)

May have more settings like background color in the future.

-}
data DisplaySettings = DisplaySettings
    { _displaySettingsScaleType :: ScaleType
    , _displaySettingsNativeResolution :: (Int, Int)
    }
makeLenses ''DisplaySettings

-- FIXME: add meta?!
{- | Only holds the relevant stateful information for the current game being played (for the deck player).

May hold inventory and other information in the future.

== Note(s)
The idea is that this is the state that is saved and loaded. In the future some parts of
this may be offloaded to a separate AppState, like `deckStateKeyTimes`.

-}
data DeckState = DeckState
    { _deckStateCurrentCard :: (AssetName, Card) -- i think it should be (assetpsth, card) fixme
    , _deckStateHoveredObjects :: HoveredObjects
    -- ^ if the string representation of an object is in here, that means it's currently
    -- hovered over. the purpose of this is to prevent re-activating hover actions before
    -- the mouse has left and re-entered.
    , _deckStateFlags :: Flags
    , _deckStateAssetRegistry :: AssetRegistry -- FIXME: should this be Maybe? It'd make saving/loading easier?
    -- ^ Like a cache of loaded assets.
    , _deckStateDisplaySettings :: DisplaySettings
    --, _deckStateKeyTimes :: KeyTimes
    }
-- TODO: do i need a object state field? or should i just use current card... yeah...
makeLenses ''DeckState -- this has to be loaded here because of issues with the fact that its used later in this same module...

{-
data AppState = AppState
    { _appStateDeckState :: DeckState
    , _appStateKeyTimes :: KeyTimes
    , _deckStateHoveredObjects :: HoveredObjects
    -- ^ if the string representation of an object is in here, that means it's currently
    -- hovered over. the purpose of this is to prevent re-activating hover actions before
    -- the mouse has left and re-entered.
    , _deckStateFlags :: Flags
    , _deckStateAssetRegistry :: AssetRegistry -- FIXME: should this be Maybe? It'd make saving/loading easier?
    -- ^ Like a cache of loaded assets.
    , _deckStateDisplaySettings :: DisplaySettings
    }
makeLenses ''AppState
-}

-- | False if flag not set, True if flag set.
checkFlag :: Flags -> String -> Bool
checkFlag flags flagName' = Set.member flagName' flags

setFlag :: Flags -> String -> Bool -> Flags
setFlag flags flagName' flagValue' =
    if flagValue'
        then
            Set.insert flagName' flags
        else
            Set.delete flagName' flags

defaultBackgroundColor :: V4 Word8
defaultBackgroundColor = V4 0 0 0 255

inHoveredObjects :: CardObject -> HoveredObjects -> Bool
inHoveredObjects obj hoveredObjects = show obj `Set.member` hoveredObjects

updateHoverStatus :: HoveredObjects -> CardObject -> HoveredObjects
updateHoverStatus hoveredObjects obj =
    if inHoveredObjects obj hoveredObjects
        then
            hoveredObjects
        else -- Trigger hover action and add to set
            Set.insert (show obj) hoveredObjects

filterOnAppearConditions :: DeckState -> [CardObject] -> [CardObject]
filterOnAppearConditions deckState' objects = do
    let flags' = deckState' ^. deckStateFlags
    (flip filter) objects $ \obj -> case obj ^. objectAppearConditions of
        Nothing -> True
        Just conditions -> and $ (flip map) conditions $ \condition ->
            case (condition ^. conditionFlag :: Maybe ActionFlag) of
                Nothing -> True
                Just (ActionFlag flagName' flagValue') -> flagValue' == Set.member flagName' flags'

