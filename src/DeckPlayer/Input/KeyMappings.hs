module DeckPlayer.Input.KeyMappings (keyMappings) where

import DeckPlayer.Input.KeyMap
import DeckPlayer.Renderer (renderToggleFullscreen)
import qualified Data.Map as Map
import DeckPlayer.TypesConstants (DeckState, deckStateDisplaySettings, displaySettingsNativeResolution)-- FIXME: move this to drawrender.hs
import Control.Lens ((^.))

keyMappings :: [KeyStateMapActionFunction DeckState]
keyMappings = [ksmafToggleFullscreen]

ksmafToggleFullscreen :: KeyStateMapActionFunction DeckState
ksmafToggleFullscreen renderer window _ _ _ deckState =
    let
        keyStateMap =
            KeyStateMap
                { keyStatePressed = renderToggleFullscreen renderer window (deckState ^. deckStateDisplaySettings . displaySettingsNativeResolution) >> pure deckState
                , keyStateRepeat = pure deckState
                , keyStateRepeatNotYet = pure deckState
                , keyStateReleased = pure deckState
                , keyStateNone = pure deckState
                }
    in
        (keyFullscreenToggle, keyStateMap)