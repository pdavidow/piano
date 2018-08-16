module Genealogy
    ( Person(..)
    , Gender(..)
    , grandparent
    , greatGrandparent
    , paternalGrandfather
    , maternalGrandfather
    , bothGrandfathers
    )

    where

-- https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe


import System.Random     
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe  
 
data Person = Person Gender deriving (Eq, Show)
data Gender = Male | Female deriving (Eq, Show)


lookupOdds :: Gender -> Int
lookupOdds gender =
    case gender of
        Male   -> 30
        Female -> 40


randomPickWithinOdds :: Int -> IO Int
randomPickWithinOdds rangeMax = do 
    gen <- getStdGen   
    let (n, _) = randomR (1, rangeMax) gen
    pure n       
    

lookupParent :: Person -> Gender -> MaybeT IO Person
lookupParent _ gender = do 
    let odds = lookupOdds gender  
    randN <- liftIO $ randomPickWithinOdds odds    
    
    if randN == odds then
        pure $ Person gender 
    else
        mzero -- MaybeT $ pure Nothing        


greatGrandparent :: Person -> Gender -> Gender -> Gender -> IO (Maybe Person) 
greatGrandparent person parentGender grandparentGender greatGrandparentGender =
    runMaybeT $ do
        p <- lookupParent person parentGender
        gp <- lookupParent p grandparentGender 
        lookupParent gp greatGrandparentGender


grandparent :: Person -> Gender -> Gender -> IO (Maybe Person) 
grandparent person parentGender grandparentGender =
    runMaybeT $ do
        parent <- lookupParent person parentGender
        lookupParent parent grandparentGender


grandfather :: Person -> Gender -> IO (Maybe Person) 
grandfather person parentGender =
    grandparent person parentGender Male


paternalGrandfather :: Person -> IO (Maybe Person) 
paternalGrandfather person = 
    grandfather person Male


maternalGrandfather :: Person -> IO (Maybe Person) 
maternalGrandfather person =
    grandfather person Female


bothGrandfathers :: Person -> IO (Maybe Person, Maybe Person)
bothGrandfathers person = do
    p <- paternalGrandfather person
    m <- maternalGrandfather person
    pure (p, m)