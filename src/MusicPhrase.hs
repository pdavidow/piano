module MusicPhrase
    ( MusicPhrase(..)
    , Style(..)
    , style
    , elagantize
    , emphasize
    )
    where

import Lib ( Emphasized, Elagantized, emphasize, elagantize )
import Instrument ( Instrument, isPolyphonic )
import Triad ( Triad )

data MusicPhrase = MusicPhrase Instrument Style Triad deriving (Eq, Show)


data Style 
    = None
    | Octave
    | OctaveRocking
    | OctaveRocking_3_1
    | Chord
    | ChordedOctave
    | Arpeggio
    | ArpeggiatedRun
    deriving (Eq, Ord, Show)


style :: MusicPhrase -> Style
style (MusicPhrase _ s _) = s


adjustStyle :: Style -> Bool -> MusicPhrase -> MusicPhrase
adjustStyle newStyle flag (MusicPhrase instrument _ triad) =
    let
        style' = if flag then newStyle else None
    in
        MusicPhrase instrument style' triad


instance Elagantized MusicPhrase where
    elagantize flag x = 
        let
            (MusicPhrase instrument _ _) = x
            instrumentedStyle = elagantizedStyle instrument
        in
            adjustStyle instrumentedStyle flag x

            
instance Emphasized MusicPhrase where
    emphasize flag x = 
        let
            (MusicPhrase instrument _ _) = x
            instrumentedStyle = emphasizedStyle instrument
        in
            adjustStyle instrumentedStyle flag x


elagantizedStyle :: Instrument -> Style
elagantizedStyle i =
    ArpeggiatedRun


emphasizedStyle :: Instrument -> Style
emphasizedStyle i =
    if isPolyphonic i then
        ChordedOctave
    else
        Arpeggio