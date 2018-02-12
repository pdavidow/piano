module InstrumentMidiNum 
    ( InstrumentMidiNum -- hiding constructor
    , InstrumentMidiNum_Invalid(..)
    , EitherIMN
    , make
    , toMidiNum 
    , InstrumentMidiNum.basicShow
    ) 
    where

import MidiNum ( MidiNum(..), basicShow )
import Instrument ( Instrument, midiNumRange )
import SpannedRange ( SpannedRange(..), inRange )


newtype InstrumentMidiNum = InstrumentMidiNum MidiNum deriving (Eq, Ord, Show)


data InstrumentMidiNum_Invalid 
    = BelowRange String
    | AboveRange String
    deriving (Eq, Show)


type EitherIMN = Either 
    InstrumentMidiNum_Invalid 
    InstrumentMidiNum


make :: Instrument -> MidiNum -> EitherIMN
make instrument midiNum = 
    let
        range = midiNumRange instrument
        (SpannedRange start end) = range
        errorString = show midiNum ++ " not in " ++ (show instrument) ++ " range [" ++ MidiNum.basicShow start ++ " .. " ++ MidiNum.basicShow end ++ "]" 
    in
        if inRange range midiNum then
            Right $ InstrumentMidiNum midiNum
        else
            if midiNum < start then 
                Left $ BelowRange errorString
            else
                Left $ AboveRange errorString


toMidiNum :: InstrumentMidiNum -> MidiNum
toMidiNum (InstrumentMidiNum n) = n


basicShow :: InstrumentMidiNum -> String
basicShow p = MidiNum.basicShow $ toMidiNum p