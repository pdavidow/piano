module Triad 
    ( Triad(..)
    , Style(..)
    , Notes -- hiding constructor
    , RootPosition(..)
    , FirstInversion(..)
    , SecondInversion(..)
    , notesFrom
    , notesFromTriad
    , rootPosition
    , firstInversion
    , secondInversion
    , arpeggiate
    )
    where

-- https://www.musictheoryacademy.com/understanding-music/triads/
-- https://www.musictheoryacademy.com/understanding-music/chord-inversions/


import MidiNum ( MidiNum(..), shiftByOctave )
import Lib ( Direction(..) )


data Notes = Notes
    { root :: MidiNum
    , third :: MidiNum
    , fifth :: MidiNum
    } deriving (Eq, Show)     


data Style = Major | Minor | Diminished | Augmented deriving (Eq, Show)


data RootPosition = RootPosition Style Notes deriving (Eq, Show)
data FirstInversion = FirstInversion Style Notes deriving (Eq, Show)
data SecondInversion = SecondInversion Style Notes deriving (Eq, Show)


data Triad
    = TriadRootPosition RootPosition
    | TriadFirstInversion FirstInversion
    | TriadSecondInversion SecondInversion
    deriving (Eq, Show)


notesFrom :: Notes -> (MidiNum, MidiNum, MidiNum)
notesFrom (Notes x1 x2 x3) = (x1, x2, x3)


notesFromTriad :: Triad -> (MidiNum, MidiNum, MidiNum)
notesFromTriad triad = 
    let
        notes = case triad of
            TriadRootPosition (RootPosition _ x) -> x
            TriadFirstInversion (FirstInversion _ x) -> x
            TriadSecondInversion (SecondInversion _ x) -> x
    in
        notesFrom notes


rootPosOffsets :: Style -> (Int, Int)
rootPosOffsets Major      =  (4, 7)
rootPosOffsets Minor      = ((fst $ rootPosOffsets Major) - 1, snd $ rootPosOffsets Major)
rootPosOffsets Diminished =  (fst $ rootPosOffsets Minor,     (snd $ rootPosOffsets Minor) - 1)
rootPosOffsets Augmented  =  (fst $ rootPosOffsets Major,     (snd $ rootPosOffsets Major) + 1)


rootPosition :: Style -> MidiNum -> RootPosition
rootPosition style root = 
    let
        (MidiNum n) = root
        (o3, o5) = rootPosOffsets style
    in
        RootPosition style $ Notes
            { fifth = MidiNum $ n + o5 
            , third = MidiNum $ n + o3
            , root = root
            }   


firstInversion :: RootPosition -> FirstInversion
firstInversion (RootPosition style (Notes n1 n3 n5)) =
    FirstInversion style $ Notes
        { root = shiftByOctave 1 Up n1
        , fifth = n5
        , third = n3
        }   
    

secondInversion :: FirstInversion -> SecondInversion
secondInversion (FirstInversion style (Notes n1 n3 n5)) =
    SecondInversion style $ Notes
        { third = shiftByOctave 1 Up n3
        , root = n1
        , fifth = n5
        }   


arpeggiate :: Direction -> Triad -> [MidiNum]
arpeggiate direction triad =
    let
        fn = case direction of
            Up -> arpeggiateUp 
            Down -> arpeggiateDown 
    in
        fn triad


arpeggiateUp :: Triad -> [MidiNum]
arpeggiateUp triad =
    case triad of 
        TriadRootPosition (RootPosition _ (Notes n1 n3 n5)) ->       [n1, n3, n5]
        TriadFirstInversion (FirstInversion _ (Notes n1 n3 n5)) ->   [n3, n5, n1]
        TriadSecondInversion (SecondInversion _ (Notes n1 n3 n5)) -> [n5, n1, n3]


arpeggiateDown :: Triad -> [MidiNum]
arpeggiateDown triad =
    reverse $ arpeggiateUp triad