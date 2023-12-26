{- | Little helper functions for the deck format.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DeckFormat.Helpers (assetsFromCard, suffixToDirectoryMapping) where

import Data.List (isSuffixOf, find)
import Data.Maybe (mapMaybe)

import DeckFormat.DeckFormat
import DeckFormat.Structure
import DeckFormat.ExtractAssets (extractAssets)

{- | Takes `extractAssets` further by bringing it closer to a representation that
the asset manager might expect.

Example:

>>> let cardBackgroundImage = CardBackgroundImage { _cardBackgroundAsset = "foo.png" }
>>> let card = Card { _cardBackgrounds = [cardBackgroundImage], _cardName = "fizz", _cardObjects = Nothing, _cardMusic = Nothing, _cardText = Nothing }
>>> assetsFromCard card
[("foo.png","backgrounds/foo.png")]
-}
assetsFromCard :: Card -> [(FilePath, FilePath)]
assetsFromCard card = mapMaybe (uncurry (flip processField)) $ extractAssets card

{- |
    Maps the ending of a record field name (JSON) to a 'DeckDirectory'.

    If a JSON field ends with one of these suffixes it's assumed to be an "asset," which
    corresponds to a specific 'DeckDirectory' (where the asset should be stored).

    This way in the JSON a user can simply specify the filename of the asset for a field
    like "cardBackgroundAsset" and the program will know to look in the corresponding
    directory.
-}
suffixToDirectoryMapping :: [(String, DeckDirectory)]
suffixToDirectoryMapping =
    [ ("BackgroundAsset", DirectoryBackgrounds)
    , ("SfxAsset", DirectorySfx)
    , ("ScriptAsset", DirectoryScripts)
    , ("ObjectAsset", DirectoryObjects)
    , ("MusicAsset", DirectoryMusic)
    , ("FontAsset", DirectoryFonts)
    , ("CursorAsset", DirectoryCursors)
    ]

{- | Takes a field name and returns the 'AssetName' and the path on disk, relative to the
deck's root directory.

Example:

>>> processField "foo.png" "someBackgroundAsset"
Just ("foo.png","backgrounds/foo.png")
-}
processField :: FilePath -> String -> Maybe (AssetName, FilePath)
processField fp fieldName =
    case find ((`isSuffixOf` fieldName) . fst) suffixToDirectoryMapping of
        Just (_, deckDirectory) -> Just (fp, deckLookup "" deckDirectory fp)
        Nothing -> Nothing