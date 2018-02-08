module PianoTriad 
    ( PianoNotes(..)
    , pianoNotesFromTriad
    )
    where

import Triad ( Triad(..), notesFromTriad )
import PianoMidiNum ( PianoMidiNum, PianoMidiNum_Invalid(..), makePianoMidiNum, basicShow )


data PianoNotes = PianoNotes
    { root :: Either PianoMidiNum_Invalid PianoMidiNum
    , third :: Either PianoMidiNum_Invalid PianoMidiNum
    , fifth :: Either PianoMidiNum_Invalid PianoMidiNum
    } deriving (Eq)    


instance Show PianoNotes where
    show (PianoNotes n1 n2 n3) = 
        let
            depict = \ invalid -> 
                case invalid of
                    BelowRange _ -> "--"
                    AboveRange _ -> "++"

            fn = \ eiPianoMidiNum -> either (\ invalid -> depict invalid) (\ valid -> basicShow valid) eiPianoMidiNum
        in
            "<" ++ fn n1 ++ " " ++ fn n2 ++ " " ++ fn n3 ++ ">"


pianoNotesFromTriad :: Triad -> PianoNotes
pianoNotesFromTriad triad =
    let
        (n1, n2, n3) = notesFromTriad triad
    in
        PianoNotes (makePianoMidiNum n1) (makePianoMidiNum n2) (makePianoMidiNum n3)



