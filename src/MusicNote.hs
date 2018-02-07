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
    } deriving (Eq, Show)