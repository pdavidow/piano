module InstrumentTriadNotes 
    ( InstrumentTriadNotes(..)
    , fromTriad
    )
    where

import Triad ( Triad(..), notesFromTriad )
import Instrument ( Instrument )
import InstrumentMidiNum ( InstrumentMidiNum, InstrumentMidiNum_Invalid(..), EitherIMN, make, basicShow )


data InstrumentTriadNotes = InstrumentTriadNotes
    { root :: EitherIMN
    , third :: EitherIMN
    , fifth :: EitherIMN
    } deriving (Eq)    
 

instance Show InstrumentTriadNotes where
    show (InstrumentTriadNotes n1 n2 n3) = 
        let
            depict = \ invalid -> 
                case invalid of
                    BelowRange _ -> "--"
                    AboveRange _ -> "++"

            fn = \ eiIMN -> either (\ invalid -> depict invalid) (\ valid -> basicShow valid) eiIMN
        in
            "<" ++ fn n1 ++ " " ++ fn n2 ++ " " ++ fn n3 ++ ">"


fromTriad :: Instrument -> Triad -> InstrumentTriadNotes
fromTriad instrument triad =
    InstrumentTriadNotes n1 n2 n3 
        where [n1, n2, n3] = map (make instrument) $ notesFromTriad triad