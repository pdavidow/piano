module MusicNote
    ( MusicNote(..), MidiNum(..), Freq(..) )
    where

newtype MidiNum = MidiNum Int deriving (Eq, Show)
newtype Freq = Freq Float deriving (Eq, Show)

data MusicNote = MusicNote 
    -- todo need https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields
    { midiNum_ :: MidiNum
    , name_ :: String
    , freq_ :: Freq
    } deriving (Eq, Show)