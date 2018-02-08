{-# LANGUAGE InstanceSigs #-}

module PianoMidiNum 
    ( PianoMidiNum -- hiding constructor
    , PianoMidiNum_Invalid(..)
    , makePianoMidiNum
    , toMidiNum 
    , PianoMidiNum.basicShow
    ) 
    where

import MidiNum ( MidiNum(..), basicShow )
import PianoNotes ( minMidiNum, maxMidiNum )


newtype PianoMidiNum = PianoMidiNum MidiNum deriving (Eq, Ord, Show)


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


toMidiNum :: PianoMidiNum -> MidiNum
toMidiNum (PianoMidiNum n) = n


basicShow :: PianoMidiNum -> String
basicShow p = MidiNum.basicShow $ toMidiNum p