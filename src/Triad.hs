module Triad 
    ( Triad(..)
    , Tone(..)
    , Notes -- hiding constructor
    , RootPosition(..)
    , FirstInversion(..)
    , SecondInversion(..)
    , notesFrom
    , notesFromTriad
    , rootPosition
    , firstInversion
    , secondInversion
    , shiftTriadBySemitone
    , shiftTriadByOctave
    , arpeggiate
    , arpeggiateRun
    )
    where

-- https://www.musictheoryacademy.com/understanding-music/triads/
-- https://www.musictheoryacademy.com/understanding-music/chord-inversions/

import Data.Range.Range ( Range, inRange )
import MidiNum ( MidiNum(..), shiftByOctave, shiftBySemitone )
import Lib ( Direction(..) )


data Notes = Notes
    { root :: MidiNum
    , third :: MidiNum
    , fifth :: MidiNum
    } deriving (Eq, Show)     


data Tone = Major | Minor | Diminished | Augmented deriving (Eq, Show)


data RootPosition = RootPosition Tone Notes deriving (Eq, Show)
data FirstInversion = FirstInversion Tone Notes deriving (Eq, Show)
data SecondInversion = SecondInversion Tone Notes deriving (Eq, Show)


data Triad
    = TriadRootPosition RootPosition
    | TriadFirstInversion FirstInversion
    | TriadSecondInversion SecondInversion
    deriving (Eq, Show)


notesFrom :: Notes -> [MidiNum]
notesFrom (Notes n1 n2 n3) = [n1, n2, n3]


notesFromTriad :: Triad -> [MidiNum]
notesFromTriad triad = 
    let
        notes = case triad of
            TriadRootPosition (RootPosition _ n) -> n
            TriadFirstInversion (FirstInversion _ n) -> n
            TriadSecondInversion (SecondInversion _ n) -> n
    in
        notesFrom notes


rootPosOffsets :: Tone -> (Int, Int)
rootPosOffsets Major      =  (4, 7)
rootPosOffsets Minor      = ((fst $ rootPosOffsets Major) - 1, snd $ rootPosOffsets Major)
rootPosOffsets Diminished =  (fst $ rootPosOffsets Minor,     (snd $ rootPosOffsets Minor) - 1)
rootPosOffsets Augmented  =  (fst $ rootPosOffsets Major,     (snd $ rootPosOffsets Major) + 1)


rootPosition :: Tone -> MidiNum -> RootPosition
rootPosition tone root = 
    let
        (MidiNum n) = root
        (o3, o5) = rootPosOffsets tone
    in
        RootPosition tone $ Notes
            { fifth = MidiNum $ n + o5 
            , third = MidiNum $ n + o3
            , root = root
            }   


firstInversion :: RootPosition -> FirstInversion
firstInversion (RootPosition tone (Notes n1 n3 n5)) =
    FirstInversion tone $ Notes
        { root = MidiNum.shiftByOctave 1 n1
        , fifth = n5
        , third = n3
        }   
    

secondInversion :: FirstInversion -> SecondInversion
secondInversion (FirstInversion tone (Notes n1 n3 n5)) =
    SecondInversion tone $ Notes
        { third = MidiNum.shiftByOctave 1 n3
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


arpeggiateRun :: Range MidiNum -> Tone -> Direction -> MidiNum -> [MidiNum]
arpeggiateRun range tone direction start = 
    let
        rootPos = rootPosition tone start
        firstInv = firstInversion rootPos

        triad = case direction of
            Up -> TriadRootPosition rootPos
            Down -> shiftTriadByOctave (-1) $ TriadFirstInversion firstInv 
    in
        if inRange range start then
            arpeggiateRecurse range direction triad
        else
            []


arpeggiateRecurse :: Range MidiNum -> Direction -> Triad -> [MidiNum]
arpeggiateRecurse range direction triad =
    let
        midiNums = arpeggiate direction triad
        filtered = filter (\ n -> inRange range n) midiNums
        isAllWithinRange = (length midiNums == length filtered)

        count = case direction of
            Up -> 1
            Down -> -1   
    in
        if isAllWithinRange then
            filtered ++ (arpeggiateRecurse range direction $ shiftTriadByOctave count triad)
        else
            filtered            


shiftTriadBySemitone :: Int -> Triad -> Triad
shiftTriadBySemitone count triad =    
    shiftTriadBy (shiftNotesBySemitone count) triad             


shiftTriadByOctave :: Int -> Triad -> Triad
shiftTriadByOctave count triad = 
    shiftTriadBy (shiftNotesByOctave count) triad   


shiftTriadBy :: (Notes -> Notes) -> Triad -> Triad
shiftTriadBy fn triad =   
    case triad of
        TriadRootPosition (RootPosition tone notes) -> TriadRootPosition (RootPosition tone $ fn notes)  
        TriadFirstInversion (FirstInversion tone notes) ->  TriadFirstInversion (FirstInversion tone $ fn notes)   
        TriadSecondInversion (SecondInversion tone notes) -> TriadSecondInversion (SecondInversion tone $ fn notes)   


shiftNotesBySemitone :: Int -> Notes -> Notes
shiftNotesBySemitone count notes =
    shiftNotesBy (MidiNum.shiftBySemitone count) notes


shiftNotesByOctave :: Int -> Notes -> Notes    
shiftNotesByOctave count notes =
    shiftNotesBy (MidiNum.shiftByOctave count) notes


shiftNotesBy :: (MidiNum -> MidiNum) -> Notes -> Notes
shiftNotesBy fn (Notes n1 n2 n3) = 
    Notes (fn n1) (fn n2) (fn n3)           