import Test.Hspec
import Data.Maybe ( fromJust )

import PianoNotes ( minPianoMidiNum, maxPianoMidiNum, nameFor, freqFor )
import MusicNote ( MidiNum(..), Freq(..) )


main :: IO ()
main = hspec $ do
    describe "PianoNotes" $ do
        describe "minPianoMidiNum" $ do
            it "minPianoMidiNum is MidiNum 21" $ do
                minPianoMidiNum `shouldBe` (MidiNum 21)
        describe "maxPianoMidiNum" $ do
            it "maxPianoMidiNum is MidiNum 108" $ do
                maxPianoMidiNum `shouldBe` (MidiNum 108)
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
 
