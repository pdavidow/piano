module MusicNote
    ( MusicNote(..), MidiNum, Freq )
    where

type MidiNum = Int -- todo data, not type
type Freq = Float -- ""

data MusicNote = MusicNote 
    { midiNum :: MidiNum
    , name :: String
    , freq :: Freq
    } deriving (Eq, Show)