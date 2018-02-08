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
    , arpeggiate
    , arpeggiateRun
    )
    where

-- https://www.musictheoryacademy.com/understanding-music/triads/
-- https://www.musictheoryacademy.com/understanding-music/chord-inversions/

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


notesFrom :: Notes -> (MidiNum, MidiNum, MidiNum)
notesFrom (Notes n1 n2 n3) = (n1, n2, n3)


notesFromTriad :: Triad -> (MidiNum, MidiNum, MidiNum)
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
        { root = shiftByOctave 1 Up n1
        , fifth = n5
        , third = n3
        }   
    

secondInversion :: FirstInversion -> SecondInversion
secondInversion (FirstInversion tone (Notes n1 n3 n5)) =
    SecondInversion tone $ Notes
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


arpeggiateRun :: [MidiNum] -> Tone -> Direction -> MidiNum -> [MidiNum]
arpeggiateRun range tone direction start = 
    let
        rootPos = rootPosition tone start
        firstInv = firstInversion rootPos

        triad = case direction of
            Up -> TriadRootPosition rootPos
            Down -> shiftTriadByOctave 1 Down $ TriadFirstInversion firstInv 

        isStartWithinRange = elem start range
    in
        if isStartWithinRange then
            arpeggiateByTriad range direction triad
        else
            []


arpeggiateByTriad :: [MidiNum] -> Direction -> Triad -> [MidiNum]
arpeggiateByTriad range direction triad =
    let
        midiNums = arpeggiate direction triad
        filtered = filter (\ n -> elem n range) midiNums
        isAllWithinRange = length midiNums == length filtered
    in
        if isAllWithinRange then
            filtered ++ (arpeggiateByTriad range direction $ shiftTriadByOctave 1 direction triad)
        else
            filtered            


-- ### todo count is abs -- try to enforce via type !!
shiftTriadByOctave :: Int -> Direction -> Triad -> Triad
shiftTriadByOctave count direction triad = 
    case triad of
        TriadRootPosition (RootPosition tone notes) ->  
            TriadRootPosition (RootPosition tone $ shiftNotesByOctave count direction notes)  

        TriadFirstInversion (FirstInversion tone notes) ->  
            TriadFirstInversion (FirstInversion tone $ shiftNotesByOctave count direction notes)   
            
        TriadSecondInversion (SecondInversion tone notes) -> 
            TriadSecondInversion (SecondInversion tone $ shiftNotesByOctave count direction notes)   

{- 
shiftTriadBySemitone :: Int -> Direction -> Triad -> Triad
shiftTriadBySemitone count direction triad =
    case triad of --todo refactor
        TriadRootPosition (RootPosition tone notes) -> 
            let
                shiftedNotes = shiftNotesBySemitone count direction notes
            in
                TriadRootPosition (RootPosition tone shiftedNotes)   

        TriadFirstInversion (FirstInversion tone notes) ->  
            let
                shiftedNotes = shiftNotesBySemitone count direction notes
            in
                TriadFirstInversion (FirstInversion tone shiftedNotes)   
            
        TriadSecondInversion (SecondInversion tone notes) -> 
            let
                shiftedNotes = shiftNotesBySemitone count direction notes
            in
                TriadSecondInversion (SecondInversion tone shiftedNotes)  -}  


-- shiftNotesBySemitone :: Int -> Direction -> Notes -> Notes
-- shiftNotesBySemitone count direction (Notes n1 n2 n3) =
--     Notes 
--         (MidiNum.shiftBySemitone count direction n1) 
--         (MidiNum.shiftBySemitone count direction n2) 
--         (MidiNum.shiftBySemitone count direction n3) 


shiftNotesByOctave :: Int -> Direction -> Notes -> Notes
shiftNotesByOctave count direction (Notes n1 n2 n3) =
    Notes 
        (MidiNum.shiftByOctave count direction n1) 
        (MidiNum.shiftByOctave count direction n2) 
        (MidiNum.shiftByOctave count direction n3)         