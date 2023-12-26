{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Audio module for DeckPlayer.

= Overview

This module is responsible for playing audio files. It uses the SDL2_mixer library.

It also features a 'MusicQueue' system.

-}
module DeckPlayer.Audio where

import Control.Monad (when)
import SDL.Mixer qualified as Mix
import DeckPlayer.Assets
import DeckFormat.DeckFormat
import Control.Lens
import DeckFormat.Structure (DeckDirectory(..))
import Data.Maybe (fromMaybe, fromJust)

{- | Play a sound effect.

= Note(s)

Will likely be renamed to 'playSfx'.

-}
playSound :: Mix.Chunk -> IO ()
playSound chunk = do
    Mix.withAudio Mix.defaultAudio 256 $ do
        -- Define a specific channel to play the sound.
        let
            channel = 1

        -- Check if the channel is currently playing a sound
        isPlaying <- Mix.playing channel

        -- If not playing, play the sound on that channel
        when (not isPlaying) $ do
            _ <- Mix.playOn channel Mix.Once chunk
            return ()

data MusicQueue = MusicQueue
    { _musicQueueCurrentSong :: Maybe CardMusic
    , _musicQueueNextSong :: Maybe CardMusic
    , _musicFinishedSignal :: Bool
    } deriving (Show)
makeLenses ''MusicQueue

initialMusicQueue :: MusicQueue
initialMusicQueue = MusicQueue
    { _musicQueueCurrentSong = Nothing
    , _musicQueueNextSong = Nothing
    , _musicFinishedSignal = False
    }

musicToAsset :: AssetRegistry -> CardMusic -> Mix.Music
musicToAsset assetRegistry cardMusic =
    let
        (AssetAudio (Music chunk)) = assetLookupError DirectoryMusic (cardMusic ^. musicMusicAsset) assetRegistry
    in
        chunk

musicDiffers :: MusicQueue -> CardMusic -> Bool
musicDiffers musicQueue cardMusic =
    case musicQueue ^. musicQueueCurrentSong of
        Nothing -> True
        Just currentSong -> currentSong /= cardMusic

{- | Main entrypoint for managing the 'MusicQueue'and playing music based off the 'MusicQueue' state.

-}
queueCheck :: MusicQueue -> AssetRegistry -> Maybe CardMusic -> IO MusicQueue
queueCheck musicQueue assetRegistry currentCardMusic = do
    musicIsPlaying <- Mix.playingMusic  -- May not need mvars at all
    let musicQueue' = musicQueue { _musicFinishedSignal = not musicIsPlaying }

    produceNewMusicQueue assetRegistry musicQueue' currentCardMusic

{- | The logic for producing a new music queue.

Conditions in which we'd change the current song (ordered by matching precedence): 

    1. the queue current song is finished
        1.1. play currentCardmusic if exists
        1.2. otherwise play next song if exists
        1.3. loop queue current song if it is finished
        1.4. blank queue
    2. the current card song has interrupt flag set true and it's not the same song as the current queue song
        2.1. play current card song and reset the queue
    3. the current card has a song and it should be added to the queue
    4. nothing happens

-}
produceNewMusicQueue :: AssetRegistry -> MusicQueue -> Maybe CardMusic -> IO MusicQueue
produceNewMusicQueue assetRegistry musicQueue currentCardMusic
    | musicQueue ^. musicFinishedSignal = do
        -- the queue current song is finished
        currentSongFinishedLogic assetRegistry musicQueue currentCardMusic
    | (fromMaybe False $ currentCardMusic >>= \x -> Just (x ^. musicInterrupt)) && musicDiffers musicQueue (fromJust currentCardMusic) = do
        -- 2. the current card song has interrupt flag set true and it's not the same song as the current queue song
        currentCardMusicInterruptLogic assetRegistry musicQueue (fromJust currentCardMusic)
    | currentCardMusic /= Nothing && musicDiffers musicQueue (fromJust currentCardMusic) = do
        -- 3. the current card has a song that's different and it should be added to the queue
        return $ musicQueue { _musicQueueNextSong = currentCardMusic }
    | otherwise = return musicQueue

currentCardMusicInterruptLogic :: AssetRegistry -> MusicQueue -> CardMusic -> IO MusicQueue
currentCardMusicInterruptLogic assetRegistry musicQueue currentCardMusic = do
    -- play the current card's music as it has the interrupt flag set and it's not the same song as the current queue song
    print "play current card, interrupt"
    Mix.haltMusic
    Mix.playMusic Mix.Once (musicToAsset assetRegistry currentCardMusic)
    return $ musicQueue { _musicFinishedSignal = False, _musicQueueCurrentSong = Just currentCardMusic, _musicQueueNextSong = Nothing }

currentSongFinishedLogic :: AssetRegistry -> MusicQueue -> Maybe CardMusic -> IO MusicQueue
currentSongFinishedLogic assetRegistry musicQueue currentCardMusic
    | currentCardMusic /= Nothing = do
        -- Play the current card's music as the current song is finished (implied?) this
        -- overrides the next song because we care more about the current room and it also
        -- wipes the next song in queue.
        print "play current card, song finished"
        let (Just song) = currentCardMusic
        Mix.haltMusic
        Mix.playMusic Mix.Once (musicToAsset assetRegistry song)
        return $ musicQueue { _musicFinishedSignal = False, _musicQueueCurrentSong = Just song, _musicQueueNextSong = Nothing }   
    | musicQueue ^. musicQueueNextSong /= Nothing = do
        -- Play the next song in queue as the current song is finished and reset the next song in queue since we've moved it to current
        print "play next song, song finished"
        let (Just song) = musicQueue ^. musicQueueNextSong
        Mix.haltMusic
        Mix.playMusic Mix.Once (musicToAsset assetRegistry song)
        return $ musicQueue { _musicFinishedSignal = False, _musicQueueCurrentSong = Just song, _musicQueueNextSong = Nothing }
    | musicQueue ^. musicQueueNextSong /= Nothing && (fromJust $ musicQueue ^. musicQueueNextSong) ^. musicLoop = do
        -- Loop the current song in queue as the current song is finished and it has the loop flag set
        print "loop current song"
        let (Just song) = musicQueue ^. musicQueueCurrentSong
        Mix.haltMusic
        Mix.playMusic Mix.Once (musicToAsset assetRegistry song)
        return $ musicQueue { _musicFinishedSignal = False, _musicQueueCurrentSong = Just song, _musicQueueNextSong = Nothing }
    | otherwise = do
        -- Blank the queue as the current song is finished and there is no next song in queue and there's no next song, either!
        print "song finished, but nothing matched"
        return $ musicQueue { _musicFinishedSignal = False, _musicQueueCurrentSong = Nothing, _musicQueueNextSong = Nothing }