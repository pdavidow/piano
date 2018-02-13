module TextPhrase
    ( TextPhrase(..)
    , Decoration(..)
    , Style(..)
    , make
    , styles
    , elagantize
    , emphasize
    )
    where

import Data.Set as Set (Set, empty, insert, delete, toList )
import Lib ( Emphasized, Elagantized, emphasize, elagantize )


data TextPhrase = TextPhrase Decoration String deriving (Eq)


data Decoration = Decoration (Set Style) deriving (Eq, Show)


data Style 
    = Bold
    | Italic
    | Underline
    | StrikeThrough
    deriving (Eq, Ord, Show)


instance Show TextPhrase where
    show (TextPhrase (Decoration xs) string) = 
        show string ++ " " ++ (show $ toList xs)


make :: String -> TextPhrase
make s =
    TextPhrase (Decoration Set.empty) s


styles :: TextPhrase -> [Style]
styles (TextPhrase (Decoration xs) _) = toList xs


adjustStyle :: Style -> Bool -> TextPhrase -> TextPhrase
adjustStyle x flag (TextPhrase (Decoration xs) string) =
    let
        fn = if flag then insert else delete
        xs' = fn x xs
    in
        TextPhrase (Decoration xs') string


instance Emphasized TextPhrase where
    emphasize flag x = 
        adjustStyle Bold flag x


instance Elagantized TextPhrase where
    elagantize flag x = 
        adjustStyle Italic flag x
