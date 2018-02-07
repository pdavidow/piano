{-# LANGUAGE InstanceSigs #-}

module PianoMidiNum 
    ( PianoMidiNum -- hiding constructor
    , PianoMidiNum_Invalid(..)
    , pianoMidiNumOn
    , midiNumFrom 
    ) 
    where

import MusicNote  ( MidiNum(..) )
import PianoNotes ( minMidiNum, maxMidiNum )


newtype PianoMidiNum = PianoMidiNum MidiNum deriving (Eq, Ord, Show)


newtype PianoMidiNum_Invalid = PianoMidiNum_Invalid String deriving (Eq, Show)


instance Bounded PianoMidiNum where
    minBound :: PianoMidiNum
    minBound = PianoMidiNum minMidiNum

    maxBound :: PianoMidiNum
    maxBound = PianoMidiNum maxMidiNum


pianoMidiNumOn :: MidiNum -> Either PianoMidiNum_Invalid PianoMidiNum
pianoMidiNumOn midiNum = 
    let
        (PianoMidiNum min) = minBound :: PianoMidiNum
        (PianoMidiNum max) = maxBound :: PianoMidiNum

        errorString = "Not in range [" ++ show min ++ " through " ++ show max ++ "]" 
    in
        if midiNum >= min && midiNum <= max then
            Right $ PianoMidiNum midiNum
        else
            Left $ PianoMidiNum_Invalid errorString


midiNumFrom :: PianoMidiNum -> MidiNum
midiNumFrom (PianoMidiNum x) = x