{-# LANGUAGE InstanceSigs #-}

module PianoMidiNum 
    ( PianoMidiNum -- hiding constructor
    , pianoMidiNumOn
    , midiNumFrom 
    ) 
    where

import MusicNote  ( MidiNum(..) )
import PianoNotes ( minMidiNum, maxMidiNum )


newtype PianoMidiNum = PianoMidiNum MidiNum deriving (Eq, Ord, Show)


instance Bounded PianoMidiNum where
    minBound :: PianoMidiNum
    minBound = PianoMidiNum minMidiNum

    maxBound :: PianoMidiNum
    maxBound = PianoMidiNum maxMidiNum


pianoMidiNumOn :: MidiNum -> Either String PianoMidiNum
pianoMidiNumOn midiNum = 
    let
        (PianoMidiNum min) = minBound :: PianoMidiNum
        (PianoMidiNum max) = maxBound :: PianoMidiNum

        errorString = "Not in range [" ++ show min ++ " through " ++ show max ++ "]" 
    in
        if midiNum >= min && midiNum <= max then
            Right $ PianoMidiNum midiNum
        else
            Left errorString


midiNumFrom :: PianoMidiNum -> MidiNum
midiNumFrom (PianoMidiNum x) = x