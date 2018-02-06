import Test.Hspec

import Data.Maybe ( fromJust )
--import Control.Exception (evaluate)

import PianoNotes ( minPianoMidiNum, maxPianoMidiNum, nameFor, freqFor )


main :: IO ()
main = hspec $ do
    describe "PianoNotes" $ do
        describe "minPianoMidiNum" $ do
            it "minPianoMidiNum is 21" $ do
                minPianoMidiNum `shouldBe` 21
        describe "maxPianoMidiNum" $ do
            it "maxPianoMidiNum is 108" $ do
                maxPianoMidiNum `shouldBe` 108
        describe "nameFor" $ do
            it "nameFor 21 is Just A0" $ do
                nameFor 21 `shouldBe` (Just "A0")
            it "nameFor 108 is Just C8" $ do
                nameFor 108 `shouldBe` (Just "C8")
            it "nameFor 20 is Nothing" $ do
                nameFor 20 `shouldBe` (Nothing :: Maybe String)
            it "nameFor 109 is Nothing" $ do
                nameFor 109 `shouldBe` (Nothing :: Maybe String)
        describe "freqFor" $ do
            it "freqFor 21 is Just 27.500" $ do
                freqFor 21 `shouldBe` (Just 27.500)
            it "freqFor 108 is Just 4186.0" $ do
                freqFor 108 `shouldBe` (Just 4186.0)
            it "freqFor 20 is Nothing" $ do
                freqFor 20 `shouldBe` (Nothing :: Maybe Float)
            it "freqFor 109 is Nothing" $ do
                freqFor 109 `shouldBe` (Nothing :: Maybe Float)



    describe "Addition2" $ do
        it "1 + 2 is greater than 2" $ do
            (1 + 2) > 2 `shouldBe` True
        it "2 + 2 is not equal to 5" $ do
            2 + 2 `shouldNotBe` 5    
