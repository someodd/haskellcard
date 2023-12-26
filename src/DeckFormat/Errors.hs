{-# LANGUAGE DeriveAnyClass #-}

-- | Errors pertaining to the deck format and structure.
module DeckFormat.Errors (CardLoadError (..), DeckLoadError (..), DeckLoader) where

import Control.Exception (Exception)
import Control.Monad.Except

type DeckLoader = ExceptT DeckLoadError IO

-- | Errors pertaining to the loading of a single card.
data CardLoadError
    = JsonParseError String
    | FileReadError String
    | OtherError String
    deriving (Show, Exception)

-- | Errors pertaining to the loading of a deck.
data DeckLoadError
    = DeckLoadCardError CardLoadError
    | DeckSanityError String
    deriving (Show, Exception)