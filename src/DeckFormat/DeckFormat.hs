{- |
    The types that represent various things in a deck including a card deck itself. Goes
    hand-in-hand with JSON. Basically a bunch of file-to-type specs.

    The format/spec of various files, their contents is described in this module.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module DeckFormat.DeckFormat where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON)
import Data.Data (Data, Typeable)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

type Flag = String

type AssetName = FilePath

{- | An 'Action' that as of right now this not only represents setting a specific flag by
name to a value (True or False), but is also used to represent using a flag as a
condition.

This may change in the future.
-}
data ActionFlag = ActionFlag
    { _flagName :: Flag
    , _flagValue :: Bool
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

-- | 'Action' to play a sound effect.
data ActionSfx = ActionSfx
    { _actionSfxAsset :: AssetName
    -- ^ The sound effect to play. Filename in the sfx directory.
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

-- | 'Action' which changes the current card to another card.
data ActionLink = ActionLink
    { _linkCard :: String
    -- ^ The card to link to. This is the filename of the card (relative to the card
    -- directory), but without the `.json` extension.
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data ActionScript = ActionScript
    { _actionScriptAsset :: AssetName
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

-- | The different kinds of actions you can take.
data Action
    = ActionTypeLink ActionLink
    | ActionTypeSfx ActionSfx
    | ActionTypeScript ActionScript
    | ActionTypeFlag ActionFlag
    | -- | This means you can have a tree of conditions, basically. Do one thing if true,
      -- but if false test for some other things.
      --
      -- This may change in the future because it doesn't make much sense if I want to make
      -- lots of things conditional to have to implement its own conditional type a part
      -- of an algebraic data type? I haven't fully decided, but am lazily defaulting on
      -- this behavior for now.
      ConditionalAction (Conditional Action)
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

-- FIXME: would it be better if conditions were instead an algebraic datatype?
data Condition = Condition
    { _conditionFlag :: Maybe ActionFlag
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

-- FIXME: I'm not sure how to best implement this for *everything* nearly like music etc
data Conditional a = Conditional
    { _conditionals :: [Condition]
    , _onTrue :: Maybe a
    , _onFalse :: Maybe a
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data CardBackgroundImage = CardBackgroundImage
    { _cardBackgroundAsset :: AssetName
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data Transformation = Transformation
    { _transformPosition :: (Int, Int)
    , _transformScale :: Float
    , _transformRotate :: Float
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data Tween = Tween
    { _tweenStart :: Transformation
    , _tweenEnd :: Transformation
    , _tweenDuration :: Int
    , _tweenOscillate :: Bool
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

type ObjectImage = FilePath

data CardObject = CardObject
    { _objectOnClick :: [Action]
    , _objectOnHover :: [Action]
    , _objectObjectAsset :: AssetName
    , _objectAppearConditions :: Maybe [Condition]
    , _objectPosition :: (Int, Int)
    , _objectTransformation :: Maybe Transformation
    , _objectTween :: Maybe Tween
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data CardMusic = CardMusic
    { _musicLoop :: Bool
    -- ^ Loop the music.
    , _musicInterrupt :: Bool
    -- ^ Let the last song finish playing before starting this music. If `False` this song
    -- will start playing immediately (abburptly interrupting anything playing).
    , _musicMusicAsset :: AssetName
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data CardText = CardText
    { _textPosition :: Maybe (Int, Int)
    , _textSize :: Maybe Int
    , _textColor :: Maybe (Int, Int, Int)
    , _textFontAsset :: Maybe AssetName
    , _text :: String
    , _textScrollbox :: Maybe Bool
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data CursorSpec = Cursor
    { _cursorSpecCursorAsset :: AssetName
    , _cursorSpecHotspot :: (Int, Int)
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data CursorDefaults = CursorDefaults
    { _cursorDefaultsHover :: CursorSpec
    , _cursorDefaultsNormal :: CursorSpec
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data TextDefaults = TextDefaults
    { _textDefaultsPosition :: (Int, Int)
    , _textDefaultsSize :: Int
    , _textDefaultsColor :: (Int, Int, Int)
    , _textDefaultsFontAsset :: AssetName
    , _textDefaultsScrollbox :: Bool
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data Card = Card
    { _cardName :: String
    , _cardBackgrounds :: [CardBackgroundImage]
    , _cardObjects :: Maybe [CardObject] -- what if NewConditional [CardObject]? instead of using onAppearConditions?
    , _cardMusic :: Maybe CardMusic
    , _cardText :: Maybe CardText -- could be [CardText]
    , _cardOnLoad :: Maybe [Action]
    , _cardEachLoop :: Maybe [Action]
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

-- | The metadata.json file contains information about the deck.
data DeckMeta = DeckMeta
    { _metaName :: String
    , _metaResolution :: (Int, Int)
    , _metaTextDefaults :: TextDefaults
    , _metaCursorDefaults :: CursorDefaults
    }
    deriving (Show, Generic, FromJSON, Eq, Data, Typeable)

data CardDeck = CardDeck
    { _deckCards :: HashMap String Card
    , _deckTitleCard :: Card
    , _deckMeta :: DeckMeta
    , _deckPath :: FilePath
    -- ^ Root path of the deck, may be a temporary directory (in the case of a zip archive
    -- being opened).
    }
    deriving (Show, Data, Typeable)

makeLenses ''Transformation
makeLenses ''Tween
makeLenses ''DeckMeta
makeLenses ''Card
makeLenses ''TextDefaults
makeLenses ''CursorDefaults
makeLenses ''CursorSpec
makeLenses ''CardText
makeLenses ''CardMusic
makeLenses ''CardObject
makeLenses ''CardBackgroundImage
makeLenses ''Condition
makeLenses ''ActionScript
makeLenses ''ActionLink
makeLenses ''ActionSfx
makeLenses ''ActionFlag
makeLenses ''CardDeck
makeLenses ''Conditional