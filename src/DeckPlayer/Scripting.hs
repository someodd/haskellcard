{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Lua scripting for decks.

Primarily, supports decks using Lua scripts to do various things.

This module integrates Lua scripting capabilities into DeckPlayer, allowing for dynamic
and flexible manipulation of resources and states. It provides a bridge between Haskell
(this game engine) and Lua, enabling scripts to interact with resources and states.

-}
module DeckPlayer.Scripting (runLuaScript) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Lens ((.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import HsLua qualified
import SDL (Renderer)
import qualified HsLua.Aeson as LuaAeson (pushValue, peekValue) 
import Data.Aeson (Value)

import Data.ByteString (ByteString)
import DeckFormat.DeckFormat (CardDeck, deckCards, deckPath, TextDefaults (..), DeckMeta(..), deckMeta, metaTextDefaults, CardText (CardText))
import DeckFormat.Structure (DeckDirectory (..))
import DeckPlayer.Assets (Asset (..), Audio(..), AssetRegistry, assetLookup, updateLoadAssetRegistry)
import DeckPlayer.Audio (playSound)
import DeckPlayer.TypesConstants (deckStateAssetRegistry)
import DeckPlayer.TypesConstants qualified as TypesConstants
import DeckPlayer.Draw
import qualified Data.Text as T


getOrUpdateAssets renderer assetRegistry' deckStateMVar deckPath deckDirectory assetPath assetConsumingFunction resultsReturned = do
    case assetLookup deckDirectory assetPath assetRegistry' of
        (Just asset) -> do
            liftIO $ assetConsumingFunction asset
            return resultsReturned
        _ -> do
            liftIO $ modifyMVar_ deckStateMVar $ \deckState -> do
                (newAssetEntry, newAssetRegistry) <-
                    updateLoadAssetRegistry
                        renderer
                        (deckState ^. deckStateAssetRegistry)
                        deckPath
                        deckDirectory
                        assetPath
                let
                    (_, asset) = newAssetEntry
                    (newDeckState :: TypesConstants.DeckState) = TypesConstants.deckStateAssetRegistry .~ newAssetRegistry $ deckState
                liftIO $ assetConsumingFunction asset
                return newDeckState
            return resultsReturned

-- FIXME
{- | Lua function for drawing text.

Basically a wrapper for 'drawText' that can be called from Lua.

drawTextDefaults
    :: Renderer
    -> TextDefaults
    -> AssetRegistry
    -> Maybe String
    -> Maybe (Int, Int, Int)
    -> Maybe (CInt, CInt)
    -> Maybe Bool
    -> T.Text
    -> IO ()

-}
drawTextLua :: Renderer -> AssetRegistry -> CardDeck -> HsLua.Lua HsLua.NumResults
drawTextLua renderer assets deck = do
    textToDraw <- HsLua.peek 1
    let textDefaults = deck ^. deckMeta . metaTextDefaults
    --_ <- liftIO $ print textDefaults
    liftIO $ drawTextDefaults renderer textDefaults assets Nothing Nothing Nothing Nothing (T.pack textToDraw)
    return 0 -- No results returned

{- | Lua mapping for 'playSound'.

= Note(s):

I'm planning to implement a difference between sound effects and music, so this
function may get renamed something like 'playSfxLua'.
-}
playSoundLua
    :: Renderer
    -> AssetRegistry
    -> CardDeck
    -> MVar TypesConstants.DeckState
    -> HsLua.Lua HsLua.NumResults
playSoundLua renderer assetRegistry cardDeck deckStateMVar = do
    soundFile <- HsLua.peek 1
    let
        assetToChunk asset = case asset of
            (AssetAudio (Sfx sfxChunk)) -> sfxChunk
            _ -> error "Asset is not audio!"
    getOrUpdateAssets
        renderer
        assetRegistry
        deckStateMVar
        (cardDeck ^. deckPath)
        DirectorySfx
        soundFile
        (playSound . assetToChunk)
        0

{- | Lua function for changing the current card in 'DeckState'.

-}
changeCurrentCardLua
    :: CardDeck -> MVar TypesConstants.DeckState -> HsLua.Lua HsLua.NumResults
changeCurrentCardLua deck deckStateMVar = do
    newCardName <- HsLua.peek 1
    liftIO $ modifyMVar_ deckStateMVar $ \deckState -> do
        let
            -- FIXME: handle Nothing! return different lua error otherwise if error
            selectedCard = fromJust $ HashMap.lookup newCardName (deck ^. deckCards)
            (newDeckState :: TypesConstants.DeckState) = TypesConstants.deckStateCurrentCard .~ (newCardName, selectedCard) $ deckState
        return newDeckState
    return 0 -- No results returned

{- | Lua function for setting a flag in 'DeckState'.

-}
setFlag :: MVar TypesConstants.DeckState -> HsLua.Lua HsLua.NumResults
setFlag deckStateMVar = do
    flagName' <- HsLua.peek 1
    flagValue' <- HsLua.peek 2
    liftIO $ modifyMVar_ deckStateMVar $ \deckState -> do
        let
            -- FIXME: handle Nothing! this is HORRIBLE! and return different lua error otherwise if error
            (newDeckState :: TypesConstants.DeckState) =
                TypesConstants.deckStateFlags
                    .~ TypesConstants.setFlag (deckState ^. TypesConstants.deckStateFlags) flagName' flagValue'
                        $ deckState
        return newDeckState
    return 0 -- No results returned

-- | Lua function for checking if a flag is set in 'DeckState'.
checkFlag :: TypesConstants.DeckState -> HsLua.Lua HsLua.NumResults
checkFlag deckState' = do
    flagName' <- HsLua.peek 1
    let
        isSet = TypesConstants.checkFlag (deckState' ^. TypesConstants.deckStateFlags) flagName'
    HsLua.pushboolean isSet
    return 1

-- THIS IS ALWAYS NILL?!!?!?
{- | Lua function for setting the Lua store in 'DeckState'.

Allows for arbitrary data from Lua to be stored in 'DeckState'.

-}
setLuaStore :: MVar TypesConstants.DeckState -> HsLua.Lua HsLua.NumResults
setLuaStore deckStateMVar = do
    -- Extract the new value from the Lua stack
    resultNewValue <- HsLua.runPeek $ LuaAeson.peekValue (-1)
    --_ <- liftIO $ print $ "hello " ++ show resultNewValue
    case resultNewValue of
        HsLua.Success val -> do
            liftIO $ modifyMVar_ deckStateMVar (\deckState -> return $ deckState { TypesConstants._deckStateLuaStore = val })
        HsLua.Failure errBytes _ -> error $ "Invalid JSON value: " ++ show errBytes
    return 0 -- Number of results returned to Lua

{- | Lua function to retrieve arbitrary Lua data from DeckState.

-}
getLuaStore :: MVar TypesConstants.DeckState -> HsLua.Lua HsLua.NumResults
getLuaStore deckStateMVar = do
    -- Read the current record from the MVar
    record <- liftIO $ readMVar deckStateMVar
    -- Push the stored Lua data onto the Lua stack
    let luaValue = TypesConstants._deckStateLuaStore record
    --_ <- liftIO $ print luaValue
    --LuaAeson.pushValue luaValue
    HsLua.pushViaJSON luaValue
    return 1 -- Number of results returned to Lua (in this case, 1 value is pushed)

{- | Run a Lua script.

This function is the main entry point for running Lua scripts.

All of the functions that are exposed to Lua are supplied in this function.

-}
runLuaScript
    :: Renderer
    -> CardDeck
    -> TypesConstants.DeckState
    -> ByteString
    -> IO TypesConstants.DeckState
runLuaScript renderer deck deckState scriptString = do
    deckStateMVar <- newMVar deckState
    let
        assetRegistry = deckState ^. TypesConstants.deckStateAssetRegistry -- FIXME: maybe this bad and should reference the deckstatemvar in all the funcs?
    status <- HsLua.run $ do
        HsLua.openlibs -- Open standard libraries

        HsLua.pushHaskellFunction (playSoundLua renderer assetRegistry deck deckStateMVar)
        HsLua.setglobal "playSound"

        HsLua.pushHaskellFunction (changeCurrentCardLua deck deckStateMVar)
        HsLua.setglobal "changeCurrentCard"

        HsLua.pushHaskellFunction (setFlag deckStateMVar)
        HsLua.setglobal "setFlag"

        HsLua.pushHaskellFunction (checkFlag deckState)
        HsLua.setglobal "checkFlag"

        HsLua.pushHaskellFunction (setLuaStore deckStateMVar)
        HsLua.setglobal "setLuaStore"

        HsLua.pushHaskellFunction (getLuaStore deckStateMVar)
        HsLua.setglobal "getLuaStore"

        HsLua.pushHaskellFunction (drawTextLua renderer assetRegistry deck)
        HsLua.setglobal "drawText"

        HsLua.dostring scriptString
    case status of
        HsLua.OK ->
            readMVar deckStateMVar
        _ -> error $ "Lua error: " ++ show status