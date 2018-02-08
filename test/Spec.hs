import Test.Hspec
import Data.Either ( isRight, isLeft, fromRight, fromLeft )

import MusicNote ( MusicNote(..) )
import PianoNotes ( minMidiNum, maxMidiNum, nameFor, freqFor )
import MidiNum ( MidiNum(..), shiftBySemitone, shiftByOctave )
import MusicNote ( Freq(..) )
import PianoMidiNum ( PianoMidiNum, PianoMidiNum_Invalid(..), makePianoMidiNum, midiNumFrom ) 
import Lib ( Direction(..) )
import Triad ( Triad(..), Style(..), notesFromTriad, rootPosition, firstInversion, secondInversion )


main :: IO ()
main = hspec $ do
    describe "MidiNum" $ do
        describe "shiftBySemitone" $ do
            it "Up 3" $ do
                (shiftBySemitone 3 Up (MidiNum 1)) `shouldBe` MidiNum 4
            it "Down 3" $ do
                (shiftBySemitone 3 Down (MidiNum 4)) `shouldBe` MidiNum 1
        describe "shiftByOctave" $ do
            it "Up 1" $ do
                (shiftByOctave 1 Up (MidiNum 1)) `shouldBe` MidiNum 13
            it "Down 1" $ do
                (shiftByOctave 1 Down (MidiNum 24)) `shouldBe` MidiNum 12
            it "Up 2" $ do
                (shiftByOctave 2 Up (MidiNum 1)) `shouldBe` MidiNum 25
            it "Down 2" $ do
                (shiftByOctave 2 Down (MidiNum 24)) `shouldBe` MidiNum 0                
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
                fromLeft (PianoMidiNum_Invalid "uhoh") (makePianoMidiNum (MidiNum 20)) `shouldBe` PianoMidiNum_Invalid "MidiNum 20 not in range [MidiNum 21 through MidiNum 108]"                        
    describe "Triad" $ do
        describe "rootPosition" $ do 
            it "Major" $ do
                (notesFromTriad $ TriadRootPosition (rootPosition Major (MidiNum 60))) `shouldBe` ((MidiNum 60), (MidiNum 64), (MidiNum 67))
            it "Minor" $ do
                (notesFromTriad $ TriadRootPosition (rootPosition Minor (MidiNum 60))) `shouldBe` ((MidiNum 60), (MidiNum 63), (MidiNum 67))
            it "Diminished" $ do
                (notesFromTriad $ TriadRootPosition (rootPosition Diminished (MidiNum 60))) `shouldBe` ((MidiNum 60), (MidiNum 63), (MidiNum 66))
            it "Augmented" $ do
                (notesFromTriad $ TriadRootPosition (rootPosition Augmented (MidiNum 60))) `shouldBe` ((MidiNum 60), (MidiNum 64), (MidiNum 68))
        describe "firstInversion" $ do 
            it "Major" $ do
                (notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Major (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 64), (MidiNum 67))
            it "Minor" $ do
                (notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Minor (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 63), (MidiNum 67))
            it "Diminished" $ do
                (notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Diminished (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 63), (MidiNum 66))
            it "Augmented" $ do
                (notesFromTriad $ TriadFirstInversion (firstInversion $ rootPosition Augmented (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 64), (MidiNum 68))
        describe "secondInversion" $ do 
            it "Major" $ do
                (notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Major (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 76), (MidiNum 67))
            it "Minor" $ do
                (notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Minor (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 75), (MidiNum 67))
            it "Diminished" $ do
                (notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Diminished (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 75), (MidiNum 66))
            it "Augmented" $ do
                (notesFromTriad $ TriadSecondInversion (secondInversion $ firstInversion $ rootPosition Augmented (MidiNum 60))) `shouldBe` ((MidiNum 72), (MidiNum 76), (MidiNum 68))    