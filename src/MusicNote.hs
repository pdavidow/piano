{-# LANGUAGE InstanceSigs #-}

module MusicNote
    ( MusicNote(..), MidiNum(..), Freq(..) )
    where

newtype MidiNum = MidiNum Int deriving (Eq, Ord, Show)
newtype Freq = Freq Float deriving (Eq, Ord, Show)

data MusicNote = MusicNote 
    -- todo https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields
    { midiNum_ :: MidiNum
    , name_ :: String
    , freq_ :: Freq
    } deriving (Show)


instance Eq MusicNote where
    (==) :: MusicNote -> MusicNote -> Bool
    (==) (MusicNote x1 _ _) (MusicNote x2 _ _) = (==) x1 x2 


instance Ord MusicNote where
    compare :: MusicNote -> MusicNote -> Ordering
    compare (MusicNote x1 _ _) (MusicNote x2 _ _) = compare x1 x2  