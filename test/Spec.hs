import Test.Hspec
import Data.Either ( isRight, isLeft, fromRight, fromLeft )

import MusicNote ( MusicNote(..) )
import PianoNotes ( minMidiNum, maxMidiNum, nameFor, freqFor )
import MidiNum ( MidiNum(..), shiftBySemitone, shiftByOctave, toInt, basicShow )
import MusicNote ( Freq(..) )
import PianoMidiNum ( PianoMidiNum, PianoMidiNum_Invalid(..), makePianoMidiNum, basicShow ) 
import Lib ( Direction(..) )
import Triad ( Triad(..), Tone(..), notesFromTriad, rootPosition, firstInversion, secondInversion, shiftTriadBySemitone, shiftTriadByOctave, arpeggiate, arpeggiateRun )
import PianoTriad  (PianoNotes(..), pianoNotesFromTriad ) 


main :: IO ()
main = hspec $ do

    describe "MidiNum" $ do
        describe "shiftBySemitone" $ do
            it "+3" $ do
                (shiftBySemitone 3 $ MidiNum 1) `shouldBe` MidiNum 4
            it "-3" $ do
                (shiftBySemitone (-3) $ MidiNum 4) `shouldBe` MidiNum 1
        describe "shiftByOctave" $ do
            it "+1" $ do
                (shiftByOctave 1 $ MidiNum 1) `shouldBe` MidiNum 13
            it "-1" $ do
                (shiftByOctave (-1) $ MidiNum 24) `shouldBe` MidiNum 12
            it "+2" $ do
                (shiftByOctave 2 $ MidiNum 1) `shouldBe` MidiNum 25
            it "-2" $ do
                (shiftByOctave (-2) $ MidiNum 24) `shouldBe` MidiNum 0   
        describe "basicShow" $ do
            it "shows only Int" $ do
                (MidiNum.basicShow (MidiNum 24)) `shouldBe` "24"

    describe "MusicNote" $ do
        describe "instance Eq" $ do 
            describe "==" $ do
                it "True" $ do
                    (MusicNote (MidiNum 1) "a" (Freq 1.1)) == (MusicNote (MidiNum 1) "b" (Freq 2.2))  `shouldBe` True
                it "False" $ do
                    (MusicNote (MidiNum 1) "a" (Freq 1.1)) == (MusicNote (MidiNum 2) "a" (Freq 1.1))  `shouldBe` False   
        describe "instance Ord" $ do  
            describe "compare" $ do
                it "LT" $ do
                    compare (MusicNote (MidiNum 1) "b" (Freq 2.2)) (MusicNote (MidiNum 2) "a" (Freq 1.1))  `shouldBe` LT   
                it "GT" $ do
                    compare (MusicNote (MidiNum 2) "a" (Freq 1.1)) (MusicNote (MidiNum 1) "b" (Freq 2.2))  `shouldBe` GT   
                it "EQ" $ do
                    compare (MusicNote (MidiNum 1) "a" (Freq 1.1)) (MusicNote (MidiNum 1) "b" (Freq 2.2))  `shouldBe` EQ             
    
    describe "PianoNotes" $ do
        describe "minMidiNum" $ do
            it "21" $ do
                minMidiNum `shouldBe` (MidiNum 21)
        describe "maxMidiNum" $ do
            it "108" $ do
                maxMidiNum `shouldBe` (MidiNum 108)
        describe "nameFor" $ do
            it "MidiNum 20 is Nothing" $ do
                nameFor (MidiNum 20) `shouldBe` (Nothing :: Maybe String)
            it "MidiNum 21 is Just A0" $ do
                nameFor (MidiNum 21) `shouldBe` (Just "A0")
            it "MidiNum 108 is Just C8" $ do
                nameFor (MidiNum 108) `shouldBe` (Just "C8")
            it "MidiNum 109 is Nothing" $ do
                nameFor (MidiNum 109) `shouldBe` (Nothing :: Maybe String)
        describe "freqFor" $ do
            it "MidiNum 20 is Nothing" $ do
                freqFor (MidiNum 20) `shouldBe` (Nothing :: Maybe Freq)
            it "MidiNum 21 is Just Freq 27.500" $ do
                freqFor (MidiNum 21) `shouldBe` (Just $ Freq 27.500)
            it "MidiNum 108 is Just Freq 4186.0" $ do
                freqFor (MidiNum 108) `shouldBe` (Just $ Freq 4186.0)
            it "MidiNum 109 is Nothing" $ do
                freqFor (MidiNum 109) `shouldBe` (Nothing :: Maybe Freq)

    describe "PianoMidiNum" $ do
        describe "instance Bounded" $ do 
            describe "minBound" $ do 
                it "at MidiNum 21" $ do  
                    -- use non-matching default
                    (minBound :: PianoMidiNum) == (fromRight (maxBound :: PianoMidiNum) (makePianoMidiNum (MidiNum 21))) `shouldBe` True
            describe "maxBound" $ do 
                it "at MidiNum 108" $ do  
                    -- use non-matching default
                    (maxBound :: PianoMidiNum) == (fromRight (minBound :: PianoMidiNum) (makePianoMidiNum (MidiNum 108))) `shouldBe` True
        describe "makePianoMidiNum" $ do
            it "valid: MidiNum 21" $ do
                isRight (makePianoMidiNum (MidiNum 21)) `shouldBe` True
            it "invalid: MidiNum 20" $ do
                isLeft (makePianoMidiNum (MidiNum 20)) `shouldBe` True   
            it "error for invalid" $ do
                fromLeft (BelowRange "uhoh") (makePianoMidiNum (MidiNum 20)) `shouldBe` BelowRange "MidiNum 20 not in range [MidiNum 21 through MidiNum 108]"     
        describe "basicShow" $ do
            it "shows only Int" $ do
                PianoMidiNum.basicShow (fromRight (maxBound :: PianoMidiNum) (makePianoMidiNum (MidiNum 60))) `shouldBe` "60"                                   
    
    describe "Triad" $ do
        describe "shiftTriadBySemitone" $ do
            it "+3" $ do
                (map toInt $ notesFromTriad $ shiftTriadBySemitone 3 $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` [63,67,70]
            it "-3" $ do
                (map toInt $ notesFromTriad $ shiftTriadBySemitone (-3) $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` [57,61,64]        
        describe "shiftTriadByOctave" $ do
            it "+3" $ do
                (map toInt $ notesFromTriad $ shiftTriadByOctave 3 $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` [96,100,103]
            it "-3" $ do
                (map toInt $ notesFromTriad $ shiftTriadByOctave (-3) $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` [24,28,31]                                   
        describe "rootPosition" $ do 
            it "Major" $ do
                (map toInt $ notesFromTriad $ TriadRootPosition (rootPosition Major (MidiNum 60))) `shouldBe` [60,64,67]
            it "Minor" $ do
                (map toInt $ notesFromTriad $ TriadRootPosition (rootPosition Minor (MidiNum 60))) `shouldBe` [60,63,67]
            it "Diminished" $ do
                (map toInt $ notesFromTriad $ TriadRootPosition (rootPosition Diminished (MidiNum 60))) `shouldBe` [60,63,66] 
            it "Augmented" $ do
                (map toInt $ notesFromTriad $ TriadRootPosition (rootPosition Augmented (MidiNum 60))) `shouldBe` [60,64,68] 
        describe "firstInversion" $ do 
            it "Major" $ do
                (map toInt $ notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Major (MidiNum 60))) `shouldBe` [72,64,67] 
            it "Minor" $ do
                (map toInt $ notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Minor (MidiNum 60))) `shouldBe` [72,63,67]
            it "Diminished" $ do
                (map toInt $ notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Diminished (MidiNum 60))) `shouldBe` [72,63,66]
            it "Augmented" $ do
                (map toInt $ notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Augmented (MidiNum 60))) `shouldBe` [72,64,68]
        describe "secondInversion" $ do 
            it "Major" $ do
                (map toInt $ notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Major (MidiNum 60))) `shouldBe` [72,76,67]
            it "Minor" $ do
                (map toInt $ notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Minor (MidiNum 60))) `shouldBe` [72,75,67] 
            it "Diminished" $ do
                (map toInt $ notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Diminished (MidiNum 60))) `shouldBe` [72,75,66]
            it "Augmented" $ do
                (map toInt $ notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Augmented (MidiNum 60))) `shouldBe` [72,76,68]     
        describe "arpeggiate" $ do    
            describe "RootPosition" $ do    
                it "Up" $ do   
                    (map toInt $ arpeggiate Up $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` [60,64,67] 
                it "Down" $ do   
                    (map toInt $ arpeggiate Down $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` [67,64,60] 
            describe "FirstInversion" $ do    
                it "Up" $ do   
                    (map toInt $ arpeggiate Up $ TriadFirstInversion $ firstInversion $ rootPosition Major $ MidiNum 60) `shouldBe` [64,67,72] 
                it "Down" $ do   
                    (map toInt $ arpeggiate Down $ TriadFirstInversion $ firstInversion $ rootPosition Major $ MidiNum 60) `shouldBe` [72,67,64] 
            describe "SecondInversion" $ do    
                it "Up" $ do   
                    (map toInt $ arpeggiate Up $ TriadSecondInversion $ secondInversion $ firstInversion $ rootPosition Major $ MidiNum 60) `shouldBe` [67,72,76] 
                it "Down" $ do   
                    (map toInt $ arpeggiate Down $ TriadSecondInversion $ secondInversion $ firstInversion $ rootPosition Major $ MidiNum 60) `shouldBe` [76,72,67] 
        describe "arpeggiateRun" $ do 
            describe "Major" $ do
                it "21 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Up $ MidiNum 21) `shouldBe` [21,25,28,33,37,40,45,49,52,57,61,64,69,73,76,81,85,88,93,97,100,105]
                it "20 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Up $ MidiNum 20) `shouldBe` []
                it "104 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Up $ MidiNum 104) `shouldBe` [104,108]               
                it "109 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Up $ MidiNum 109) `shouldBe` []  
                it "108 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Down $ MidiNum 108) `shouldBe` [108,103,100,96,91,88,84,79,76,72,67,64,60,55,52,48,43,40,36,31,28,24] 
                it "109 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Down $ MidiNum 109) `shouldBe` [] 
                it "24 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Down $ MidiNum 24) `shouldBe` [24] 
                it "21 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Down $ MidiNum 21) `shouldBe` [21] 
                it "20 Down, piano bounded" $ do
                     (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Major Down $ MidiNum 20) `shouldBe` [] 
            describe "Minor" $ do
                it "21 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Up $ MidiNum 21) `shouldBe` [21,24,28,33,36,40,45,48,52,57,60,64,69,72,76,81,84,88,93,96,100,105,108]
                it "20 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Up $ MidiNum 20) `shouldBe` []
                it "104 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Up $ MidiNum 104) `shouldBe` [104,107]               
                it "109 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Up $ MidiNum 109) `shouldBe` []  
                it "108 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Down $ MidiNum 108) `shouldBe` [108,103,99,96,91,87,84,79,75,72,67,63,60,55,51,48,43,39,36,31,27,24] 
                it "109 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Down $ MidiNum 109) `shouldBe` [] 
                it "24 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Down $ MidiNum 24) `shouldBe` [24] 
                it "21 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Down $ MidiNum 21) `shouldBe` [21] 
                it "20 Down, piano bounded" $ do
                        (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Minor Down $ MidiNum 20) `shouldBe` [] 
            describe "Diminished" $ do
                it "21 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Up $ MidiNum 21) `shouldBe` [21,24,27,33,36,39,45,48,51,57,60,63,69,72,75,81,84,87,93,96,99,105,108]
                it "20 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Up $ MidiNum 20) `shouldBe` []
                it "104 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Up $ MidiNum 104) `shouldBe` [104,107]               
                it "109 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Up $ MidiNum 109) `shouldBe` []  
                it "108 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Down $ MidiNum 108) `shouldBe` [108,102,99,96,90,87,84,78,75,72,66,63,60,54,51,48,42,39,36,30,27,24] 
                it "109 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Down $ MidiNum 109) `shouldBe` [] 
                it "24 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Down $ MidiNum 24) `shouldBe` [24] 
                it "21 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Down $ MidiNum 21) `shouldBe` [21] 
                it "20 Down, piano bounded" $ do
                        (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Diminished Down $ MidiNum 20) `shouldBe` [] 
            describe "Augmented" $ do
                it "21 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Up $ MidiNum 21) `shouldBe` [21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97,101,105]
                it "20 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Up $ MidiNum 20) `shouldBe` []
                it "104 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Up $ MidiNum 104) `shouldBe` [104,108]               
                it "109 Up, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Up $ MidiNum 109) `shouldBe` []  
                it "108 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Down $ MidiNum 108) `shouldBe` [108,104,100,96,92,88,84,80,76,72,68,64,60,56,52,48,44,40,36,32,28,24] 
                it "109 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Down $ MidiNum 109) `shouldBe` [] 
                it "24 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Down $ MidiNum 24) `shouldBe` [24] 
                it "21 Down, piano bounded" $ do
                    (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Down $ MidiNum 21) `shouldBe` [21] 
                it "20 Down, piano bounded" $ do
                        (map toInt $ arpeggiateRun [minMidiNum..maxMidiNum] Augmented Down $ MidiNum 20) `shouldBe` [] 


    describe "PianoNotes" $ do     
        describe "instance Show" $ do      
            describe "show" $ do    
                it "valid" $ do
                    (show $ pianoNotesFromTriad $ TriadRootPosition $ rootPosition Major $ MidiNum 60) `shouldBe` "<60 64 67>"
                it "partially BelowRange" $ do
                    (show $ pianoNotesFromTriad $ TriadRootPosition $ rootPosition Major $ MidiNum 20) `shouldBe` "<-- 24 27>"
                it "partially AboveRange" $ do
                    (show $ pianoNotesFromTriad $ TriadRootPosition $ rootPosition Major $ MidiNum 104) `shouldBe` "<104 108 ++>"
                it "totally BelowRange" $ do
                    (show $ pianoNotesFromTriad $ TriadRootPosition $ rootPosition Major $ MidiNum 10) `shouldBe` "<-- -- -->"
                it "totally AboveRange" $ do
                    (show $ pianoNotesFromTriad $ TriadRootPosition $ rootPosition Major $ MidiNum 110) `shouldBe` "<++ ++ ++>"