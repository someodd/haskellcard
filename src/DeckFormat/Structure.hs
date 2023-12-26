{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The directory and general structure of a card deck. This module is not concerned with
the contents of the files, only the general (file/directory) layout of the card deck.

A game is stored as a series of 'Cards' in a 'CardDeck'. A 'CardDeck' is a zip archive
which contains the cards and their assets (images, music, etc.).  The structure looks like
this:

    somegame.carddeck.zip
    ├── metadata.json
    ├── cards/
    │   ├── title.card.json
    │   └── somecard.card.json
    ├── cursors/
    │   ├── hover.png
    │   └── normal.png
    ├── backgrounds/
    │   ├── background1.png
    │   ├── background2.png
    │   └── hills.jpg
    ├── sfx/
    │   └── sfx.ogg
    ├── music/
    │   └── song.ogg
    ├── fonts/
    │   └── roboto.ttf
    ├── scripts/
    │   └── sound.lua
    ├── objects/
    └── └── key.png

Some files are required like `title.card.json` and `metadata.json`.
The game starts on `title.card.json`.
-}
module DeckFormat.Structure (loadCardDeck, deckLookup, DeckDirectory (..), titleCardName) where

import Codec.Archive.Zip qualified as Zip
import Control.Lens
import Control.Monad (filterM, forM)
import Control.Monad.Except (ExceptT, runExceptT, throwError, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (lefts, partitionEithers)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List (isSuffixOf)
import Data.Maybe (isJust, maybeToList)
import GHC.Generics (Generic)
import System.Directory (
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath (takeBaseName, takeExtensions, (</>))
import System.IO.Error (catchIOError)

import DeckFormat.DeckFormat (Card, CardDeck (CardDeck), DeckMeta)
import DeckFormat.Errors (
    CardLoadError (FileReadError, JsonParseError, OtherError),
    DeckLoadError (..),
    DeckLoader,
 )

{- | The different directories in a card deck.

Each directory has a special role for a card deck.
-}
data DeckDirectory
    = DirectoryCards
    | DirectoryBackgrounds
    | DirectorySfx
    | DirectoryMusic
    | DirectoryObjects
    | DirectoryCursors
    | DirectoryFonts
    | DirectoryScripts
    | DirectoryRoot
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Hashable)

-- | List of all the directories in a card deck except the root directory.
allDirectoriesExceptRoot :: [DeckDirectory]
allDirectoriesExceptRoot = filter (/= DirectoryRoot) [minBound .. maxBound]

-- | Specification/meta for a 'DeckDirectory'.
data DeckDirectoryMeta = DeckDirectoryMeta
    { _directoryName :: FilePath
    -- ^ The actual name/path of the directory on disk, relative to the root of the card deck.
    , _directoryMandatoryFiles :: [FilePath]
    -- ^ List of files that must exist in the directory.
    , _directoryMandatoryDirectories :: [DeckDirectory]
    -- ^ List of directories that must exist in the directory.
    , _directoryExtensionWhitelist :: Maybe [String]
    -- ^ Only these files with these extensions are allowed to exist in the deck. In the
    -- future I want to move beyond just extensions and actually be able to name the specs
    -- from 'DeckFormat'.
    }
    deriving (Show)

makeLenses ''DeckDirectoryMeta

{- | List of all the image formats supported by the engine.

For example, for background images or object images.

Note:
    .zip is my custom animation format. Maybe I should give it a different extension?
-}
supportedImageFormats :: [String]
supportedImageFormats = [".jpg", ".png", ".zip"]

-- | List of all the audio formats supported by the engine for sound effects and music.
supportedAudioFormats :: [String]
supportedAudioFormats = [".ogg", ".mp3"]

-- The name of the title card without the .json
titleCardName :: String
titleCardName = "title.card"

{- | Map of 'DeckDirectory' to 'DeckDirectoryMeta'.

Maps what's basically a spec/meta for a directory to the directory itself.

This should prove all the 'DeckDirectory's are in 'metaHelper':

>>> all (`elem` HashMap.keys metaHelper) [minBound .. maxBound]
True
-}
metaMap :: [(DeckDirectory, DeckDirectoryMeta)]
metaMap =
    [
        ( DirectoryCards
        , DeckDirectoryMeta "cards" [titleCardName ++ ".json"] [] (Just [".card.json"]) -- FIXME: card.json
        )
    ,
        ( DirectoryBackgrounds
        , DeckDirectoryMeta "backgrounds" [] [] (Just supportedImageFormats)
        )
    , (DirectorySfx, DeckDirectoryMeta "sfx" [] [] (Just supportedAudioFormats))
    , (DirectoryMusic, DeckDirectoryMeta "music" [] [] (Just supportedAudioFormats))
    , (DirectoryObjects, DeckDirectoryMeta "objects" [] [] (Just supportedImageFormats))
    , (DirectoryCursors, DeckDirectoryMeta "cursors" [] [] (Just [".png"]))
    , (DirectoryFonts, DeckDirectoryMeta "fonts" [] [] (Just [".ttf"]))
    , (DirectoryScripts, DeckDirectoryMeta "scripts" [] [] (Just [".lua"]))
    ,
        ( DirectoryRoot
        , DeckDirectoryMeta "" ["metadata.json"] allDirectoriesExceptRoot (Just [".json", ".md", ".txt"])
        )
    ]

-- | 'HashMap.HashMap' of 'metaMap'.
metaHelper :: HashMap.HashMap DeckDirectory DeckDirectoryMeta
metaHelper = HashMap.fromList metaMap

{- | Get the appropriate path for a file in a card deck.

Will throw an error if there was a lookup failure. This behavior is because *all* of the
`DeckDirectory`s should be in `metaHelper`, associated with rules/meta, otherwise this is
a neglectful bug in the engine.

Examples:

>>> deckLookup "/home/user/carddeck" DirectoryCards "title.card.json"
"/home/user/carddeck/cards/title.card.json"
>>> deckLookup "/home/user/carddeck" DirectoryBackgrounds "background1.png"
"/home/user/carddeck/backgrounds/background1.png"
>>> deckLookup "/home/user/carddeck" DirectoryRoot "metadata.json"
"/home/user/carddeck/metadata.json"
-}
deckLookup
    :: FilePath
    -- ^ The path to the card deck root.
    -> DeckDirectory
    -- ^ The directory to look in.
    -> FilePath
    -- ^ The file name to look for.
    -> FilePath
deckLookup deckRoot directory fileName =
    case HashMap.lookup directory metaHelper of
        Just directoryMeta ->
            deckRoot </> directoryMeta ^. directoryName </> fileName
        Nothing -> error $ "Neglected to map a directory to meta: " ++ show directory

{- | Get the title card from a deck.

The title card is a mandatory card for a 'CardDeck'. The title card is the
first 'Card' that is shown when the game starts.
-}
getTitleCard :: HashMap.HashMap String Card -> Either DeckLoadError Card
getTitleCard deck =
    case HashMap.lookup titleCardName deck of
        Just card -> Right card
        Nothing -> Left $ DeckLoadCardError $ OtherError "No title card found"

{- | Ensure that the card deck directory/file structure is valid according to
all the 'DeckDirectoryMeta's in 'metaMap'.

Will give back all the errors ('DeckDirectoryMeta' rule violations) it finds, or just ().
-}
cardDeckSanityCheck :: FilePath -> IO (Either DeckLoadError ())
cardDeckSanityCheck cardDeckRootPath = do
    -- For every 'DirectoryMeta' (hopefully) ensure its rules are followed.
    errors <- forM metaMap $ \(_, meta) -> do
        -- FIXME: should be using DeckDirectory (fst)? or just meta since that's rule-checking?
        -- Dynamically get the directory path from the meta then check its rules.
        let
            dirPath = cardDeckRootPath </> meta ^. directoryName
        existsDir <- doesDirectoryExist dirPath
        if not existsDir
            then return $ Left ("Missing directory: " ++ dirPath)
            else do
                fileChecks <- mapM (checkFileExists dirPath) (meta ^. directoryMandatoryFiles)
                whitelistCheck <-
                    if isJust $ meta ^. directoryExtensionWhitelist
                        then checkExtensionWhitelist dirPath (meta ^. directoryExtensionWhitelist)
                        else return Nothing
                let
                    missingFiles = lefts fileChecks
                    whitelistError = maybeToList whitelistCheck
                return
                    $ if null missingFiles && null whitelistError
                        then Right ()
                        else Left (unlines $ missingFiles ++ whitelistError)
    return
        $ if null (lefts errors) then Right () else Left (DeckSanityError $ unlines (lefts errors))

{- | Check if a file exists in a directory.

Not simply using `doesFileExist` because I want to handle/make an error if
the file doesn't exist.
-}
checkFileExists :: FilePath -> FilePath -> IO (Either String ())
checkFileExists dir file = do
    let
        fullPath = dir </> file
    exists <- doesFileExist fullPath
    return $ if exists then Right () else Left ("Missing file: " ++ fullPath)

{- | Check if the extensions of the files in a directory are in a whitelist.

In other words, ensure that only the whitelisted file extensions are present in the
directory.
-}
checkExtensionWhitelist :: FilePath -> Maybe [String] -> IO (Maybe String)
checkExtensionWhitelist dirPath whitelist = case whitelist of
    Just exts -> do
        files <- listDirectory dirPath
        filesNotDirectories <- filterM doesFileExist (map (dirPath </>) files)
        let
            invalidFiles = filter (\file -> takeExtensions file `notElem` exts) filesNotDirectories
        return
            $ if null invalidFiles
                then Nothing
                else Just $ "Invalid file extensions in " ++ dirPath ++ ": " ++ unwords invalidFiles
    Nothing -> return Nothing

{- | Load a card deck from a path.

The path may be a directory or a zip file.
-}
loadCardDeck :: FilePath -> IO (Either DeckLoadError CardDeck)
loadCardDeck baseFilePath = do
    exists <- doesDirectoryExist baseFilePath
    if exists
        then loadDataFromDirectory baseFilePath
        else do
            let
                zipFilePath = baseFilePath ++ ".carddeck.zip"
            isZipFile <- doesFileExist zipFilePath
            if isZipFile
                then catchIOError (loadDataFromZip zipFilePath) handleIOError
                else
                    return
                        . Left
                        $ DeckLoadCardError
                        $ FileReadError
                        $ "Neither a directory nor a zip file exists for: "
                        ++ baseFilePath

{- | Basically a wrapper for 'loadDataFromDirectory', but it can extract a zip file to a
temporary directory, then pass that directory to 'loadDataFromDirectory'.
-}
loadDataFromZip :: FilePath -> IO (Either DeckLoadError CardDeck)
loadDataFromZip filePath = do
    tempDir <- getTemporaryDirectory >>= (`createUniqueTempDir` "carddeck")
    bytes <- B.readFile filePath
    let
        archive = Zip.toArchive bytes
    Zip.extractFilesFromArchive [Zip.OptDestination tempDir] archive
    result <- loadDataFromDirectory tempDir
    --removeDirectoryRecursive tempDir
    return result

createUniqueTempDir :: FilePath -> String -> IO FilePath
createUniqueTempDir tempDir prefix = do
    let
        dirPath = tempDir </> prefix
    createDirectory dirPath
    return dirPath

{- | Load a card deck from a directory.

This is the main function for loading a card deck.
-}
loadDataFromDirectory :: FilePath -> IO (Either DeckLoadError CardDeck)
loadDataFromDirectory dirPath = runExceptT $ do
    -- Perform a sanity check on the directory.
    liftIO (cardDeckSanityCheck dirPath) >>= either throwError return

    -- Load metadata from the directory.
    meta <-
        liftIO (loadMetadata $ deckLookup dirPath DirectoryRoot "metadata.json")
            >>= either throwError return

    -- Check if the cards directory exists. Throw an error if it doesn't.
    let
        cardsDirPath = deckLookup dirPath DirectoryCards ""
    exists <- liftIO $ doesDirectoryExist cardsDirPath
    unless exists
        $ throwError
        $ DeckLoadCardError
        $ FileReadError
        $ "Cards directory does not exist at: "
        ++ cardsDirPath

    -- Get all the card file paths in the cards directory.
    cardFiles <- liftIO $ listDirectory cardsDirPath
    let
        jsonCardFiles = filter (".card.json" `isSuffixOf`) cardFiles

    -- For each card file, read its contents and decode it.
    -- The results are tuples with filename and either an error or a card.
    cardsResults <- forM jsonCardFiles $ \file -> do
        let
            fullPath = cardsDirPath </> file
        cardData <- liftIO $ BL.readFile fullPath
        return (takeBaseName file, eitherDecode cardData)

    -- Separate the results into errors and successful decodings.
    -- 'partitionEithers' splits a list of 'Either' into two lists of 'Left' and 'Right'.
    let
        (errors, cardPairs) =
            partitionEithers
                [ either (Left . (,) name) (Right . (,) name) errorOrCard
                | (name, errorOrCard) <- cardsResults
                ]
    -- If there are any errors, throw them.
    unless (null errors)
        $ throwError
        $ DeckLoadCardError
        $ JsonParseError
        $ unlines (map (\(name, errorString) -> name ++ ": \n" ++ errorString) errors)

    -- Create a card deck from the successfully decoded cards.
    let
        deck = HashMap.fromList cardPairs
    -- Check if the title card exists in the deck. If not, throw an error.
    titleCard <- either throwError return $ getTitleCard deck
    -- Finally, return the complete card deck.
    return $ CardDeck deck titleCard meta dirPath

loadMetadata :: FilePath -> IO (Either DeckLoadError DeckMeta)
loadMetadata metaPath = do
    metaData <- catchIOError (BL.readFile metaPath) (const $ return B.empty)
    case eitherDecode metaData of
        Left err -> return $ Left (DeckLoadCardError $ JsonParseError err)
        Right meta -> return $ Right meta

handleIOError :: IOError -> IO (Either DeckLoadError CardDeck)
handleIOError e = return . Left $ DeckLoadCardError $ FileReadError $ show e
