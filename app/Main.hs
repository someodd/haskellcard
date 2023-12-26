import Data.Typeable (typeOf)
import System.Environment (getArgs)
import System.Directory (
    removeDirectoryRecursive
 )

import DeckFormat.Structure (loadCardDeck)
import DeckPlayer.DeckPlayer (playDeck)


main :: IO ()
main = do
    deckPath <- head <$> getArgs
    result <- loadCardDeck deckPath
    (deck, maybeTempFilePath) <- case result of
        Left err -> error $ "DECK LOAD ERROR: " ++ (show . typeOf $ err) ++ "\n" ++ show err
        Right deck -> pure deck
    playDeck deck
    case maybeTempFilePath of
        Nothing -> pure ()
        Just tempDir -> removeDirectoryRecursive tempDir