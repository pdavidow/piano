module PianoMidiNum 
    ( PianoMidiNum -- hiding constructor
    , pianoMidiNumOn
    , midiNumFrom 
    ) 
    where

-- import Control.Exception ( assert ) -- see below...
import MusicNote  ( MidiNum(..) )
import PianoNotes ( minMidiNum, maxMidiNum )


newtype PianoMidiNum = PianoMidiNum MidiNum deriving (Eq, Ord, Show)


instance Bounded PianoMidiNum where
    minBound = PianoMidiNum minMidiNum
    maxBound = PianoMidiNum maxMidiNum


pianoMidiNumOn :: MidiNum -> PianoMidiNum
pianoMidiNumOn midiNum = 
    -- https://wiki.haskell.org/Smart_constructors
    let
        (PianoMidiNum min) = minBound :: PianoMidiNum
        (PianoMidiNum max) = maxBound :: PianoMidiNum
    in
        -- alas, this doesn't test well in Hspec: evaluate (pianoMidiNumOn (MidiNum 20)) `shouldThrow` anyException
        -- assert (midiNum >= min && midiNum <= max) 
        --     $ PianoMidiNum midiNum

        -- but this does:
        if midiNum >= min && midiNum <= max then
            PianoMidiNum midiNum
        else
            error $ "Invalid MidiNum" 


midiNumFrom :: PianoMidiNum -> MidiNum
midiNumFrom (PianoMidiNum midiNum) = midiNum