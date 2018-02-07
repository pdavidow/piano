import Test.Hspec
import Control.Exception ( evaluate )

import MusicNote ( MusicNote(..) )
import PianoNotes ( minMidiNum, maxMidiNum, nameFor, freqFor )
import MusicNote ( MidiNum(..), Freq(..) )
import PianoMidiNum ( PianoMidiNum, pianoMidiNumOn, midiNumFrom ) 

main :: IO ()
main = hspec $ do
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
            it "MidiNum 21 is Just A0" $ do
                nameFor (MidiNum 21) `shouldBe` (Just "A0")
            it "MidiNum 108 is Just C8" $ do
                nameFor (MidiNum 108) `shouldBe` (Just "C8")
            it "MidiNum 20 is Nothing" $ do
                nameFor (MidiNum 20) `shouldBe` (Nothing :: Maybe String)
            it "MidiNum 109 is Nothing" $ do
                nameFor (MidiNum 109) `shouldBe` (Nothing :: Maybe String)
        describe "freqFor" $ do
            it "MidiNum 21 is Just Freq 27.500" $ do
                freqFor (MidiNum 21) `shouldBe` (Just $ Freq 27.500)
            it "MidiNum 108 is Just Freq 4186.0" $ do
                freqFor (MidiNum 108) `shouldBe` (Just $ Freq 4186.0)
            it "MidiNum 20 is Nothing" $ do
                freqFor (MidiNum 20) `shouldBe` (Nothing :: Maybe Freq)
            it "MidiNum 109 is Nothing" $ do
                freqFor (MidiNum 109) `shouldBe` (Nothing :: Maybe Freq)
    describe "PianoMidiNum" $ do
        describe "instance Bounded" $ do 
            describe "minBound" $ do 
                it "at MidiNum 21" $ do       
                    (minBound :: PianoMidiNum) == (pianoMidiNumOn (MidiNum 21)) `shouldBe` True
            describe "maxBound" $ do 
                it "at MidiNum 108" $ do       
                    (maxBound :: PianoMidiNum) == (pianoMidiNumOn (MidiNum 108)) `shouldBe` True
        describe "pianoMidiNumOn" $ do
            it "MidiNum 21 valid" $ do
                midiNumFrom (pianoMidiNumOn (MidiNum 21)) `shouldBe` MidiNum 21
            it "MidiNum 20 throws exception" $ do
                evaluate (pianoMidiNumOn (MidiNum 20)) `shouldThrow` anyException

