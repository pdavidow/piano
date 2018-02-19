{-# LANGUAGE InstanceSigs #-}

module MusicNote
    ( MusicNote(..)
    , Freq(..) 
    )
    where

import MidiNum ( MidiNum(..) )


newtype Freq = Freq Float deriving (Eq, Ord, Show)


data MusicNote = MusicNote 
    { note_midiNum :: MidiNum
    , note_name :: String
    , note_freq :: Freq
    } deriving (Show)


instance Eq MusicNote where
    (==) :: MusicNote -> MusicNote -> Bool
    (==) (MusicNote x1 _ _) (MusicNote x2 _ _) = (==) x1 x2 


instance Ord MusicNote where
    compare :: MusicNote -> MusicNote -> Ordering
    compare (MusicNote x1 _ _) (MusicNote x2 _ _) = compare x1 x2  