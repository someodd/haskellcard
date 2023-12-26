import Data.Typeable (typeOf)
import System.Environment (getArgs)


import DeckFormat.Structure (loadCardDeck)
import DeckPlayer.DeckPlayer (playDeck)

main :: IO ()
main = do
    deckPath <- head <$> getArgs
    result <- loadCardDeck deckPath
    deck <- case result of
        Left err -> error $ "DECK LOAD ERROR: " ++ (show . typeOf $ err) ++ "\n" ++ show err
        Right deck -> pure deck
    playDeck deck
    -- FIXME: SHOULD REMOVE THE TEMP FILE ZIP WAS EXTRACTED TO IF READ FROM ZIP