-- FIXME: proper module documentation.
{- | Abstraction for input.

Currently only supports keyboard input.

Is done polymorphically.

The abstractions are useful for controlling aspects like repeat delay and repeat rate.

== Note(s)

This module may be useful for SDL users in general, perhaps I should make it into a
package!

-}
module DeckPlayer.Input.KeyMap where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified SDL
import SDL (Renderer, Window, Scancode (unwrapScancode))
import Data.Word (Word32)
import Data.List (foldl')
import Control.Monad (foldM)
import SDL.Input (Scancode)

-- FIXME...
keyFullscreenToggle :: [Scancode]
keyFullscreenToggle = [SDL.ScancodeRAlt, SDL.ScancodeReturn]

{- | Stores key press information as a HashMap.

The Word32 is just the unwrapped Scancode. The float is the time it was pressed.

Stores keys in lists because of key combos.

-}
type KeyTimes = HashMap.HashMap [Word32] Float

emptyKeyTimes :: KeyTimes
emptyKeyTimes = HashMap.empty

-- FIXME: should be moved or set in some kind of config
pauseBeforeDelay :: Float
pauseBeforeDelay = 0.5

{- | Mapping of a key's various states to values.

This allows for different actions to be taken when a key is pressed, released, etc.

-}
data KeyStateMap a = KeyStateMap
    { keyStatePressed :: a
    -- ^ The key was just/first pressed.
    , keyStateRepeat :: a
    -- ^ The key was pressed and is being held down. It is repeating.
    , keyStateRepeatNotYet :: a
    -- ^ The key was pressed and is being held down. It is not yet repeating. This is due
    -- to a set delay between when the key is first pressed and when it starts
    -- repeating/is considered to be repeating.
    , keyStateNone :: a
    -- ^ The key is not pressed.
    , keyStateReleased :: a
    -- ^ The key was just released and is now no longer being held down.
    }

{- | Puts `KeyStateMap` into action by using it to update the `KeyTimes` HashMap (which
stores special key states/"better" key info) and also give the corresponding value for the
corresponding key state which is active.

@param keys A function that takes a `SDL.Scancode` and returns a `Bool` indicating if the
    key is pressed or not. Standard SDL stuff.
@param keyTimes The `KeyTimes` HashMap to update.
@param scancode The `SDL.Scancode` to check. The key whose state we're checking various
    states thereof in order to determine such a state's corresponding value from the
    provided `KeyStateMap`.
@param currentTime The current time in milliseconds. Used to determine if a key is
    repeating or not.
@param keyStateMap The `KeyStateMap` to use to determine the value of the key state
    that the key is in.
@return A tuple containing the value corresponding to the current key state and the
    updated `KeyTimes` HashMap.

== Example

If the key is first/just pressed, the `keyStatePressed` value from the map will be
returned. We also get back the new `KeyTimes` HashMap based off said occurence.

>>> let keyTimes = HashMap.empty
>>> let keys = (== SDL.ScancodeA)
>>> let scancode = SDL.ScancodeA
>>> let currentTime = 0
>>> let keyStateMap = KeyStateMap { keyStatePressed = 1, keyStateRepeat = 2, keyStateRepeatNotYet = 3, keyStateReleased = 4, keyStateNone = 5 }
>>> keyTimeMap keys keyTimes scancode currentTime keyStateMap
(1,fromList [(4,0.0)])

== Other notes

This kind of abstraction also allows for the actual specific key assigned to be separate
from various key state/value mappings.

-}
keyTimeMap
    :: (SDL.Scancode -> Bool)
    -> KeyTimes
    -> [Scancode]
    -> Float
    -> KeyStateMap a
    -> (a, KeyTimes)
keyTimeMap keys keyTimes scancodes currentTime keyStateMap =
    if all keys scancodes
        then case HashMap.lookup (map unwrapScancode scancodes) keyTimes of
            -- It's not in the HashMap, meaning it's the first press. We now add the time it was pressed
            -- to the HashMap.
            Nothing ->
                ( keyStatePressed keyStateMap
                , HashMap.insert (map unwrapScancode scancodes) currentTime keyTimes
                )
            -- It's in the HashMap, meaning it's a repeat. We check if it's time to repeat.
            Just lastTime ->
                ( if (currentTime - lastTime) >= pauseBeforeDelay
                    then keyStateRepeat keyStateMap
                    else keyStateRepeatNotYet keyStateMap
                , keyTimes
                )
        else -- If is actually deleted from the HashMap (and it was in there), that means it was released. Although
        -- to be sure it was actually released, the logic will have to be tested with HashMap.delete (seeing if
        -- it actually deleted anything).
        case HashMap.lookup (map unwrapScancode scancodes) keyTimes of
            Nothing -> (keyStateNone keyStateMap, keyTimes)
            Just _ -> (keyStateReleased keyStateMap, HashMap.delete (map unwrapScancode scancodes) keyTimes)

type KeyStateMapActionFunction a
    = Renderer
    -> Window
    -> (SDL.Scancode -> Bool)
    -> Word32
    -> Float
    -> a
    -> ([Scancode], KeyStateMap (IO a))

performKeyStateMapActionFunction
    :: KeyStateMapActionFunction a
    -> Renderer
    -> Window
    -> (SDL.Scancode -> Bool)
    -> Word32
    -> Float
    -> KeyTimes
    -> a
    -> IO (a, KeyTimes)
performKeyStateMapActionFunction keyStateMapActionFunction renderer window keys currentTime deltaTime keyTimes a = do
    let
        (keysToMatch, keyStateMap) = keyStateMapActionFunction renderer window keys currentTime deltaTime a
        (newAppStateIO, newKeyTimes) =
            keyTimeMap keys keyTimes keysToMatch (fromIntegral currentTime) keyStateMap
    newAppState <- newAppStateIO
    pure (newAppState, newKeyTimes)

performAllKeyStateMapActionFunctions
    :: Renderer -> Window -> (Scancode -> Bool) -> Word32 -> Float -> KeyTimes -> [KeyStateMapActionFunction a] -> a -> IO (a, KeyTimes)
performAllKeyStateMapActionFunctions renderer window keys currentTime deltaTime keyTimes listOfKeyStateMapActionFunctions a = do
    foldM
        ( \(a_, keyTimes_) action ->
            performKeyStateMapActionFunction action renderer window keys currentTime deltaTime keyTimes_ a_
        )
        (a, keyTimes)
        listOfKeyStateMapActionFunctions
