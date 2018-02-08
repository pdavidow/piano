{-# LANGUAGE InstanceSigs #-}

module PianoMidiNum 
    ( PianoMidiNum -- hiding constructor
    , PianoMidiNum_Invalid(..)
    , makePianoMidiNum
    , midiNumFrom 
    , PianoMidiNum.basicShow
    ) 
    where

import MidiNum ( MidiNum(..), basicShow )
import PianoNotes ( minMidiNum, maxMidiNum )


newtype PianoMidiNum = PianoMidiNum MidiNum deriving (Eq, Ord, Show)


-- todo add constructors BelowRange | AboveRange
data PianoMidiNum_Invalid 
    = BelowRange String
    | AboveRange String
    deriving (Eq, Show)


instance Bounded PianoMidiNum where
    minBound :: PianoMidiNum
    minBound = PianoMidiNum minMidiNum

    maxBound :: PianoMidiNum
    maxBound = PianoMidiNum maxMidiNum


makePianoMidiNum :: MidiNum -> Either PianoMidiNum_Invalid PianoMidiNum
makePianoMidiNum midiNum = 
    let
        (PianoMidiNum min) = minBound :: PianoMidiNum
        (PianoMidiNum max) = maxBound :: PianoMidiNum

        errorString = show midiNum ++ " not in range [" ++ show min ++ " through " ++ show max ++ "]" 
    in
        if midiNum >= min && midiNum <= max then
            Right $ PianoMidiNum midiNum
        else
            if midiNum < min then 
                Left $ BelowRange errorString
            else
                Left $ AboveRange errorString


midiNumFrom :: PianoMidiNum -> MidiNum
midiNumFrom (PianoMidiNum x) = x


basicShow :: PianoMidiNum -> String
basicShow (PianoMidiNum midiNum) = MidiNum.basicShow midiNum