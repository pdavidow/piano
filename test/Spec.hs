import Test.Hspec
import Control.Exception ( evaluate )

import MusicNote ( MusicNote(..) )
import PianoNotes ( minMidiNum, maxMidiNum, nameFor, freqFor )
import MusicNote ( MidiNum(..), Freq(..) )
import PianoMidiNum ( PianoMidiNum, pianoMidiNumOn, midiNumFrom ) 

main :: IO ()
main = hspec $ do
    describe "MusicNote" $ do
        describe "==" $ do
            it "True" $ do
                (MusicNote (MidiNum 1) "a" (Freq 1.1)) == (MusicNote (MidiNum 1) "b" (Freq 2.2))  `shouldBe` True
            it "False" $ do
                (MusicNote (MidiNum 1) "a" (Freq 1.1)) == (MusicNote (MidiNum 2) "a" (Freq 1.1))  `shouldBe` False    
        describe "compare" $ do
            it "LT" $ do
                compare (MusicNote (MidiNum 1) "b" (Freq 2.2)) (MusicNote (MidiNum 2) "a" (Freq 1.1))  `shouldBe` LT   
            it "GT" $ do
                compare (MusicNote (MidiNum 2) "a" (Freq 1.1)) (MusicNote (MidiNum 1) "b" (Freq 2.2))  `shouldBe` GT   
            it "EQ" $ do
                compare (MusicNote (MidiNum 1) "a" (Freq 1.1)) (MusicNote (MidiNum 1) "b" (Freq 2.2))  `shouldBe` EQ             
    describe "PianoNotes" $ do
        describe "minMidiNum" $ do
            it "minMidiNum is MidiNum 21" $ do
                minMidiNum `shouldBe` (MidiNum 21)
        describe "maxMidiNum" $ do
            it "maxMidiNum is MidiNum 108" $ do
                maxMidiNum `shouldBe` (MidiNum 108)
        describe "nameFor" $ do
            it "nameFor MidiNum 21 is Just A0" $ do
                nameFor (MidiNum 21) `shouldBe` (Just "A0")
            it "nameFor MidiNum 108 is Just C8" $ do
                nameFor (MidiNum 108) `shouldBe` (Just "C8")
            it "nameFor MidiNum 20 is Nothing" $ do
                nameFor (MidiNum 20) `shouldBe` (Nothing :: Maybe String)
            it "nameFor MidiNum 109 is Nothing" $ do
                nameFor (MidiNum 109) `shouldBe` (Nothing :: Maybe String)
        describe "freqFor" $ do
            it "freqFor MidiNum 21 is Just Freq 27.500" $ do
                freqFor (MidiNum 21) `shouldBe` (Just $ Freq 27.500)
            it "freqFor MidiNum 108 is Just Freq 4186.0" $ do
                freqFor (MidiNum 108) `shouldBe` (Just $ Freq 4186.0)
            it "freqFor MidiNum 20 is Nothing" $ do
                freqFor (MidiNum 20) `shouldBe` (Nothing :: Maybe Freq)
            it "freqFor MidiNum 109 is Nothing" $ do
                freqFor (MidiNum 109) `shouldBe` (Nothing :: Maybe Freq)
    describe "PianoMidiNum" $ do
        describe "pianoMidiNumOn" $ do
            it "pianoMidiNumOn (MidiNum 21) is valid" $ do
                midiNumFrom (pianoMidiNumOn (MidiNum 21)) `shouldBe` MidiNum 21
            it "pianoMidiNumOn (MidiNum 20) throws exception" $ do
                evaluate (pianoMidiNumOn (MidiNum 20)) `shouldThrow` anyException

