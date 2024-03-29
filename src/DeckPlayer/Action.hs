{- | This module is responsible for handling the more interactive parts of the deck.

This includes collisions.

-}
module DeckPlayer.Action where

import SDL (Renderer)
import Control.Lens
import Foreign.C (CInt)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import DeckPlayer.Audio (playSound)
import DeckPlayer.Scripting (runLuaScript)
import Control.Lens ((^.))
import DeckPlayer.TweenTransform
import Data.Maybe (fromJust, isJust)

import DeckPlayer.TypesConstants
import DeckPlayer.Assets
import DeckFormat.DeckFormat
import DeckFormat.Structure (DeckDirectory(..), titleCardName, deckLookup)
import Control.Monad (foldM)
import Data.Word (Word32)

{- | A wrapper for performing a series of actions, accumulating the state. 

-}
handleActions :: Renderer -> [Action] -> CardDeck -> Word32 -> DeckState -> IO DeckState
handleActions renderer actions deck currentTicks deckState =
    foldM (\state action -> handleAction renderer action deck currentTicks state) deckState actions

{- | Handle/perform an 'Action', possibly conditionally.

-}
handleAction :: Renderer -> Action -> CardDeck -> Word32 -> DeckState -> IO DeckState
handleAction renderer actionOrConditional deck currentTicks deckState =
    case actionOrConditional of
        (ConditionalAction (Conditional conditionals actionOnTrue actionOnFalse)) -> do
            -- In the future there will be more conditionals and we'll basically do an AND match on them.
            let
                -- Maybe there's a better way to do this?
                -- need to do this as for condition in conditionals
                conditionalBoolMap = flip map conditionals $ \condition -> do
                    let conditionalFlag' = condition ^. conditionFlag
                    -- Apply these conditions if not Nothing
                    (conditionalFlag', performFlagConditional)
                conditionsToApply = map (\s -> snd s $ fromJust $ fst s) $ filter (isJust . fst) conditionalBoolMap
            if and conditionsToApply
                then do
                    case actionOnTrue of
                        Just actionOnTrue' ->
                            handleAction' actionOnTrue' deck currentTicks deckState
                        Nothing ->
                            return deckState
                else do
                    case actionOnFalse of
                        Just actionOnFalse' -> handleAction' actionOnFalse' deck currentTicks deckState
                        Nothing -> return deckState

        action -> handleAction' action deck currentTicks deckState
   where
    performFlagConditional (ActionFlag requireThisFlag requireThisFlagValue) =
        let
            flagActualValue = requireThisFlag `Set.member` (deckState ^. deckStateFlags)
        in
            flagActualValue == requireThisFlagValue

    handleAction' :: Action -> CardDeck -> Word32 -> DeckState -> IO DeckState
    handleAction' action deck currentTicks deckState =
        case action of
            (ActionTypeLink (ActionLink linkCard')) -> do
                let
                    newCard = HashMap.lookup linkCard' (deck ^. deckCards)
                case newCard of
                    Just card -> return $ deckState & deckStateCurrentCard .~ (linkCard', card)
                    Nothing -> return deckState
            (ActionTypeSfx (ActionSfx sfx)) -> do 
                let (AssetAudio (Sfx sfxChunk)) = assetLookupError DirectorySfx sfx (deckState ^. deckStateAssetRegistry)
                playSound sfxChunk
                return deckState
            (ActionTypeScript script) -> do
                let (AssetScript scriptAsset) = assetLookupError DirectoryScripts (script ^. actionScriptAsset) (deckState ^. deckStateAssetRegistry)
                runLuaScript renderer (fromIntegral currentTicks :: Int) deck deckState scriptAsset
            (ActionTypeFlag (ActionFlag flagName' flagValue')) -> do
                return $ deckStateFlags .~ setFlag (deckState ^. deckStateFlags) flagName' flagValue' $ deckState
            conditionalAction@(ConditionalAction _) ->
                handleAction renderer conditionalAction deck currentTicks deckState

{- | Checks if the cordinate '(Cint, Cint)' is inside of 'CardObject'.
    
This accounts for 'Transformation'.

-}
isInsideObject :: Renderer -> AssetRegistry -> FilePath -> (CInt, CInt) -> CardObject -> IO Bool
isInsideObject _ assetRegistry _ (x, y) obj = do
    ((objX, objY), (objW, objH), _) <- applyTransformation assetRegistry obj
    let (objX', objY') = (fromIntegral objX, fromIntegral objY)
        (objW', objH') = (fromIntegral objW, fromIntegral objH)
    pure $ x >= objX' && x <= objX' + objW' && y >= objY' && y <= objY' + objH'
