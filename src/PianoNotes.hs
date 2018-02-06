module PianoNotes 
    ( pianoNotesList, minPianoMidiNum, maxPianoMidiNum, nameFor, freqFor )
    where

import MusicNote ( MusicNote(..), MidiNum, Freq )
import Data.List ( find )

minPianoMidiNum = midiNum $ head pianoNotesList
maxPianoMidiNum = midiNum $ last pianoNotesList


musicNoteFor :: MidiNum -> Maybe MusicNote
musicNoteFor n =
    find (\ musicNote -> midiNum musicNote == n) pianoNotesList

datumFor :: MidiNum -> (MusicNote -> a) -> Maybe a
datumFor n fn =
    maybe Nothing (\ musicNote -> Just $ fn musicNote) $ musicNoteFor n 

nameFor :: MidiNum -> Maybe String
nameFor n =
    datumFor n name

freqFor :: MidiNum -> Maybe Freq
freqFor n =
    datumFor n freq 


pianoNotesList :: [] MusicNote
pianoNotesList =
    -- http://newt.phys.unsw.edu.au/jw/notes.html
    [ MusicNote 21 "A0"     27.500
    , MusicNote 22 "A0#"            29.135
    , MusicNote 23 "B0"     30.868 
    , MusicNote 24 "C1"     32.703 
    , MusicNote 25 "C1#"            34.648 
    , MusicNote 26 "D1"     36.708 
    , MusicNote 27 "D1#"            38.891 
    , MusicNote 28 "E1"     41.203 
    , MusicNote 29 "F1"     43.654 
    , MusicNote 30 "F1#"            46.249 
    , MusicNote 31 "G1"     48.999 
    , MusicNote 32 "G1#"            51.913 
    , MusicNote 33 "A1"     55.000 
    , MusicNote 34 "A1#"            58.270 
    , MusicNote 35 "B1"     61.735 
    , MusicNote 36 "C2"     65.406 
    , MusicNote 37 "C2#"            69.296 
    , MusicNote 38 "D2"     73.416 
    , MusicNote 39 "D2#"            77.782 
    , MusicNote 40 "E2"     82.407 
    , MusicNote 41 "F2"     87.307 
    , MusicNote 42 "F2#"            92.499 
    , MusicNote 43 "G2"     97.999 
    , MusicNote 44 "G2#"            103.83 
    , MusicNote 45 "A2"     110.00 
    , MusicNote 46 "A2#"            116.54 
    , MusicNote 47 "B2"     123.47 
    , MusicNote 48 "C3"     130.81 
    , MusicNote 49 "C3#"            138.59 
    , MusicNote 50 "D3"     146.83 
    , MusicNote 51 "D3#"            155.56 
    , MusicNote 52 "E3"     164.81 
    , MusicNote 53 "F3"     174.61 
    , MusicNote 54 "F3#"            185.00 
    , MusicNote 55 "G3"     196.00 
    , MusicNote 56 "G3#"            207.65 
    , MusicNote 57 "A3"     220.00 
    , MusicNote 58 "A3#"            233.08 
    , MusicNote 59 "B3"     246.94 
    , MusicNote 60 "C4"     261.63  ------------- Middle C
    , MusicNote 61 "C4#"            277.18 
    , MusicNote 62 "D4"     293.67 
    , MusicNote 63 "D4#"            311.13 
    , MusicNote 64 "E4"     329.63 
    , MusicNote 65 "F4"     349.23 
    , MusicNote 66 "F4#"            369.99 
    , MusicNote 67 "G4"     392.00 
    , MusicNote 68 "G4#"            415.30 
    , MusicNote 69 "A4"     440.00 
    , MusicNote 70 "A4#"            466.16 
    , MusicNote 71 "B4"     493.88 
    , MusicNote 72 "C5"     523.25 
    , MusicNote 73 "C5#"            554.37 
    , MusicNote 74 "D5"     587.33 
    , MusicNote 75 "D5#"            622.25 
    , MusicNote 76 "E5"     659.26 
    , MusicNote 77 "F5"     698.46 
    , MusicNote 78 "F5#"            739.99 
    , MusicNote 79 "G5"     783.99 
    , MusicNote 80 "G5#"            830.61 
    , MusicNote 81 "A5"     880.00 
    , MusicNote 82 "A5#"            932.33 
    , MusicNote 83 "B5"     987.77 
    , MusicNote 84 "C6"     1046.5 
    , MusicNote 85 "C6#"            1108.7 
    , MusicNote 86 "D6"     1174.7 
    , MusicNote 87 "D6#"            1244.5 
    , MusicNote 88 "E6"     1318.5 
    , MusicNote 89 "F6"     1396.9 
    , MusicNote 90 "F6#"            1480.0 
    , MusicNote 91 "G6"     1568.0 
    , MusicNote 92 "G6#"            1661.2 
    , MusicNote 93 "A6"     1760.0 
    , MusicNote 94 "A6#"            1864.7 
    , MusicNote 95 "B6"     1975.5 
    , MusicNote 96 "C7"     2093.0 
    , MusicNote 97 "C7#"            2217.5 
    , MusicNote 98 "D7"     2349.3 
    , MusicNote 99 "D7#"            2489.0 
    , MusicNote 100 "E7"    2637.0 
    , MusicNote 101 "F7"    2793.0 
    , MusicNote 102 "F7#"           2960.0 
    , MusicNote 103 "G7"    3136.0 
    , MusicNote 104 "G7#"           3322.4 
    , MusicNote 105 "A7"    3520.0 
    , MusicNote 106 "A7#"           3729.3 
    , MusicNote 107 "B7"    3951.1 
    , MusicNote 108 "C8"    4186.0 
    ]