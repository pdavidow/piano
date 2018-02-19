module PianoNotes 
    ( pianoNotesList
    , nameFor
    , freqFor 
    )
    where

import Data.List ( find )

import MidiNum ( MidiNum(..) )
import MusicNote ( MusicNote(..), Freq(..) )


musicNoteFor :: MidiNum -> Maybe MusicNote
musicNoteFor midiNum =
    find (\ musicNote -> note_midiNum musicNote == midiNum) pianoNotesList


datumFor :: (MusicNote -> a) -> MidiNum -> Maybe a
datumFor fn midiNum =
    maybe Nothing (\ musicNote -> Just $ fn musicNote) $ musicNoteFor midiNum 


nameFor :: MidiNum -> Maybe String
nameFor midiNum =
    datumFor note_name midiNum 


freqFor :: MidiNum -> Maybe Freq
freqFor midiNum =
    datumFor note_freq midiNum 


pianoNotesList :: [MusicNote]
pianoNotesList =
    -- http://newt.phys.unsw.edu.au/jw/notes.html
    [ MusicNote (MidiNum 21) "A0"     (Freq 27.500)
    , MusicNote (MidiNum 22) "A0#"            (Freq 29.135)
    , MusicNote (MidiNum 23) "B0"     (Freq 30.868) 
    , MusicNote (MidiNum 24) "C1"     (Freq 32.703) 
    , MusicNote (MidiNum 25) "C1#"            (Freq 34.648) 
    , MusicNote (MidiNum 26) "D1"     (Freq 36.708) 
    , MusicNote (MidiNum 27) "D1#"            (Freq 38.891) 
    , MusicNote (MidiNum 28) "E1"     (Freq 41.203) 
    , MusicNote (MidiNum 29) "F1"     (Freq 43.654) 
    , MusicNote (MidiNum 30) "F1#"            (Freq 46.249) 
    , MusicNote (MidiNum 31) "G1"     (Freq 48.999) 
    , MusicNote (MidiNum 32) "G1#"            (Freq 51.913) 
    , MusicNote (MidiNum 33) "A1"     (Freq 55.000) 
    , MusicNote (MidiNum 34) "A1#"            (Freq 58.270) 
    , MusicNote (MidiNum 35) "B1"     (Freq 61.735) 
    , MusicNote (MidiNum 36) "C2"     (Freq 65.406) 
    , MusicNote (MidiNum 37) "C2#"            (Freq 69.296) 
    , MusicNote (MidiNum 38) "D2"     (Freq 73.416) 
    , MusicNote (MidiNum 39) "D2#"            (Freq 77.782) 
    , MusicNote (MidiNum 40) "E2"     (Freq 82.407) 
    , MusicNote (MidiNum 41) "F2"     (Freq 87.307) 
    , MusicNote (MidiNum 42) "F2#"            (Freq 92.499) 
    , MusicNote (MidiNum 43) "G2"     (Freq 97.999) 
    , MusicNote (MidiNum 44) "G2#"            (Freq 103.83) 
    , MusicNote (MidiNum 45) "A2"     (Freq 110.00) 
    , MusicNote (MidiNum 46) "A2#"            (Freq 116.54) 
    , MusicNote (MidiNum 47) "B2"     (Freq 123.47) 
    , MusicNote (MidiNum 48) "C3"     (Freq 130.81) 
    , MusicNote (MidiNum 49) "C3#"            (Freq 138.59) 
    , MusicNote (MidiNum 50) "D3"     (Freq 146.83) 
    , MusicNote (MidiNum 51) "D3#"            (Freq 155.56) 
    , MusicNote (MidiNum 52) "E3"     (Freq 164.81) 
    , MusicNote (MidiNum 53) "F3"     (Freq 174.61) 
    , MusicNote (MidiNum 54) "F3#"            (Freq 185.00) 
    , MusicNote (MidiNum 55) "G3"     (Freq 196.00) 
    , MusicNote (MidiNum 56) "G3#"            (Freq 207.65) 
    , MusicNote (MidiNum 57) "A3"     (Freq 220.00) 
    , MusicNote (MidiNum 58) "A3#"            (Freq 233.08) 
    , MusicNote (MidiNum 59) "B3"     (Freq 246.94) 
    , MusicNote (MidiNum 60) "C4"     (Freq 261.63)  ------------- Middle C
    , MusicNote (MidiNum 61) "C4#"            (Freq 277.18) 
    , MusicNote (MidiNum 62) "D4"     (Freq 293.67) 
    , MusicNote (MidiNum 63) "D4#"            (Freq 311.13) 
    , MusicNote (MidiNum 64) "E4"     (Freq 329.63) 
    , MusicNote (MidiNum 65) "F5"     (Freq 349.23)
    , MusicNote (MidiNum 65) "F4"     (Freq 349.23) 
    , MusicNote (MidiNum 66) "F4#"            (Freq 369.99) 
    , MusicNote (MidiNum 67) "G4"     (Freq 392.00) 
    , MusicNote (MidiNum 68) "G4#"            (Freq 415.30) 
    , MusicNote (MidiNum 69) "A4"     (Freq 440.00) 
    , MusicNote (MidiNum 70) "A4#"            (Freq 466.16) 
    , MusicNote (MidiNum 71) "B4"     (Freq 493.88) 
    , MusicNote (MidiNum 72) "C5"     (Freq 523.25) 
    , MusicNote (MidiNum 73) "C5#"            (Freq 554.37) 
    , MusicNote (MidiNum 74) "D5"     (Freq 587.33) 
    , MusicNote (MidiNum 75) "D5#"            (Freq 622.25) 
    , MusicNote (MidiNum 76) "E5"     (Freq 659.26) 
    , MusicNote (MidiNum 77) "F5"     (Freq 698.46) 
    , MusicNote (MidiNum 78) "F5#"            (Freq 739.99) 
    , MusicNote (MidiNum 79) "G5"     (Freq 783.99) 
    , MusicNote (MidiNum 80) "G5#"            (Freq 830.61) 
    , MusicNote (MidiNum 81) "A5"     (Freq 880.00) 
    , MusicNote (MidiNum 82) "A5#"            (Freq 932.33) 
    , MusicNote (MidiNum 83) "B5"     (Freq 987.77) 
    , MusicNote (MidiNum 84) "C6"     (Freq 1046.5) 
    , MusicNote (MidiNum 85) "C6#"            (Freq 1108.7) 
    , MusicNote (MidiNum 86) "D6"     (Freq 1174.7) 
    , MusicNote (MidiNum 87) "D6#"            (Freq 1244.5) 
    , MusicNote (MidiNum 88) "E6"     (Freq 1318.5) 
    , MusicNote (MidiNum 89) "F6"     (Freq 1396.9) 
    , MusicNote (MidiNum 90) "F6#"            (Freq 1480.0) 
    , MusicNote (MidiNum 91) "G6"     (Freq 1568.0) 
    , MusicNote (MidiNum 92) "G6#"            (Freq 1661.2) 
    , MusicNote (MidiNum 93) "A6"     (Freq 1760.0) 
    , MusicNote (MidiNum 94) "A6#"            (Freq 1864.7) 
    , MusicNote (MidiNum 95) "B6"     (Freq 1975.5) 
    , MusicNote (MidiNum 96) "C7"     (Freq 2093.0) 
    , MusicNote (MidiNum 97) "C7#"            (Freq 2217.5) 
    , MusicNote (MidiNum 98) "D7"     (Freq 2349.3) 
    , MusicNote (MidiNum 99) "D7#"            (Freq 2489.0) 
    , MusicNote (MidiNum 100) "E7"    (Freq 2637.0) 
    , MusicNote (MidiNum 101) "F7"    (Freq 2793.0) 
    , MusicNote (MidiNum 102) "F7#"           (Freq 2960.0) 
    , MusicNote (MidiNum 103) "G7"    (Freq 3136.0) 
    , MusicNote (MidiNum 104) "G7#"           (Freq 3322.4) 
    , MusicNote (MidiNum 105) "A7"    (Freq 3520.0)
    , MusicNote (MidiNum 106) "A7#"           (Freq 3729.3) 
    , MusicNote (MidiNum 107) "B7"    (Freq 3951.1)
    , MusicNote (MidiNum 108) "C8"    (Freq 4186.0)
    ]