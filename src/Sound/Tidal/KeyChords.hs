{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Sound.Tidal.KeyChords where

{-
   This is HoshiNoRandii's module to implement chords in TidalCycles in
   a way that makes it easier to stay in a desired key without knowing
   a lot of music theory.
-}

import           Data.Maybe
import           Data.List
import           Data.Tuple
import           Sound.Tidal.Types
import           Sound.Tidal.Scales
import           Sound.Tidal.Pattern
import           Sound.Tidal.Params

------ types, classes, and related functions ------

-- Degree type
-- for indicating scale degree when you don't know what scale you are using
-- includes the degree and a semitone adjustment
data Degree = Degree { deg :: Int
                     , semi :: Int
                     } deriving (Show)

-- degAdd function to easily add to the degree field of a Degree
degAdd :: Degree -> Int -> Degree
degAdd (Degree d st) i = Degree (d+i) st

-- octAdd function to easily add an octave (12 semitones) to a Degree
octAdd :: Degree -> Int -> Degree
octAdd (Degree d st) i = Degree d (st+12*i)

-- semiAdd function to easily add to the semitone field of a Degree
semiAdd :: Degree -> Int -> Degree
semiAdd (Degree d st) i = Degree d (st+i)

-- degrees can be compared
instance Eq Degree where 
   (Degree d1 s1) == (Degree d2 s2) 
      = (d1 == d2) && (s1 == s2)

-- octEquiv checks if two Degrees are equivalent up to octave
-- 12 semitones = 1 octave
octEquiv :: Degree -> Degree -> Bool
octEquiv (Degree d1 s1) (Degree d2 s2)
   = (d1 == d2) && ((s1 `mod` 12) == (s2 `mod` 12))

instance PartialOrd Degree where
   partLE (Degree d1' s1') (Degree d2' s2')
      =  (d1 <= d2) && (s1 <= s2)
      || (d1 > d2) && (s1 <= s2 - scaleMaxGap*(d1 - d2)) 
      || (d1 <= d2 - (s1-s2)/scaleMinGap) && (s1 > s2)
         where d1 = fromIntegral d1'
               s1 = fromIntegral s1'
               d2 = fromIntegral d2'
               s2 = fromIntegral s2'

-- gaps returns a list of differences between adjacent elements in 
-- the given list
-- takes the absolute value of the differences
gaps :: (Num a) => [a] -> [a]
gaps [] = []
gaps (x:xs)
   | length xs < 1 = []
   | otherwise     = difference:(gaps xs)
      where difference = abs $ (xs!!0) - x

-- maxGap returns the largest difference between adjacent elements in
-- the given list
maxGap :: (Num a, Ord a) => [a] -> a
maxGap list = maximum $ gaps list

-- minGap returns the smallest difference between adjacent elements
-- in the given list
minGap :: (Num a, Ord a) => [a] -> a
minGap list = minimum $ gaps list

-- scaleMaxGap calculates the largest semitone gap between scale degrees
-- that can be found in the scaleTable
scaleMaxGap :: (Fractional a, Ord a) => a
scaleMaxGap = maximum $ map (maxGap . snd) scaleTable

-- scaleMinGap calculates the smallest semitone gap between scale degrees
-- that can be found thin the scaleTable
scaleMinGap :: (Fractional a, Ord a) => a
scaleMinGap = minimum $ map (minGap . snd) scaleTable

class PartialOrd a where
   partLE :: a -> a -> Bool
   partGE :: a -> a -> Bool
   partLT :: a -> a -> Bool
   partGT :: a -> a -> Bool
   partEQ :: a -> a -> Bool
   partNE :: a -> a -> Bool
   partCompare :: a -> a -> Maybe Ordering

   partGE x y = partLE y x
   partLT x y = (partLE x y) && (partNE x y)
   partGT x y = (partGE x y) && (partNE x y)
   partEQ x y = (partLE x y) && (partGE x y)
   partNE x y = not $ partEQ x y
   partCompare x y
      | partLT x y = Just LT
      | partGT x y = Just GT
      | partEQ x y = Just EQ
      | otherwise  = Nothing

-- isTopSorted checks if a list of partially ordered elements
-- is in a valid topological sorting
isTopSorted :: (PartialOrd a) => [a] -> Bool
isTopSorted [] = True
isTopSorted (x:xs) 
   | length xs == 0 = True
   | otherwise      = sorted x (xs!!0) && isTopSorted xs

-- sorted checks if two elements are in a valid topological sorting
sorted :: (PartialOrd a) => a -> a -> Bool
sorted x y
   | x `partLE` y                 = True
   | x `partCompare` y == Nothing = True
   | otherwise                    = False

-- topSort returns a valid topological sorting of a list of
-- partially ordered elements
topSort :: (PartialOrd a) => [a] -> [a]
topSort list 
   | isTopSorted list = list
   | otherwise        = mins ++ (topSort remaining)
      where
         mins = minima list
         remaining = filter (flip partNotElem mins) list

-- minima returns the list of all elements which are not greater than
-- any other element of the list
minima :: (PartialOrd a) => [a] -> [a]
minima list = filter (\x -> all (not . partGT x) list) list

-- partElem checks if an element is a member of the structure
-- using the partial order notion of equality
partElem :: (Foldable t, PartialOrd a) => a -> t a -> Bool
partElem x xs = foldr f False xs
   where f = foldComp x

-- foldComp is a helper function for partElem
-- returns True if it is passed True
-- or if x `partEQ` y
-- the Bool it is passed represents whether the element has occurred
-- in the structure yet
foldComp :: (PartialOrd a) => a -> a -> Bool -> Bool
foldComp x y bool = bool || (x `partEQ` y)

-- partNotElem is the negation of partElem
partNotElem :: (Foldable t, PartialOrd a) => a -> t a -> Bool
partNotElem x xs = not $ partElem x xs

-- DegChord type
-- a chord of scale degrees
-- includes the root of the chord
-- and a list of the other scale degrees in the chord
data DegChord = DegChord { degRoot :: Degree
                         , degList :: [Degree]
                         } deriving (Show)

-- degTriad takes a Degree sclDeg
-- and returns a DegChord which is a triad built 
-- on the given scale degree
-- entries in degList are in ascending order
degTriad :: Degree -> DegChord
degTriad sclDeg = DegChord { degRoot = sclDeg
                           , degList = sclDeg:third:fifth:[]
                           }
                  where
                     third = sclDeg `degAdd` 2
                     fifth = sclDeg `degAdd` 4 

-- Key type
-- includes the tonic and the "scale" (mode)
data Key = Key { tonic :: Note
               , mode :: [Double] -- TODO: figure out how to have "Fractional a => [a]" here, to match Scales.hs
               } deriving (Show)

-- another constructor for Keys
-- strKey takes a Note ton as the tonic
-- and a String modeStr with the name of a scale (mode)
-- and contructs a Key
strKey :: Note -> String -> Key
strKey ton modeStr 
   | jMode /= Nothing = Key {tonic = ton, mode = (fromJust jMode)}
   | otherwise        = error "Mode not available"
   where jMode = lookup modeStr scaleTable

-- DCMod type
-- modifiers for DegChords
data DCMod = DInvert | DOpen | DPower | DUp | DDown 
           | DAdd AddWhere (Maybe Int) Int deriving Eq

instance Show DCMod where
   show DInvert = "DegChord Invert"
   show DOpen = "DegChord Open"
   show DPower = "Power DegChord"
   show DUp = "DegChord Octave Up"
   show DDown = "DegChord Octave Down"
   show (DAdd x i st) = "DegChord Add Scale Degree" ++ show i ++ "+" ++ show st ++ "semitones to" ++ show x

-- AddWhere type
-- to indicate adding tones to the treble or bass of a chord
data AddWhere = Treble | Bass deriving Eq

instance Show AddWhere where
   show Treble = "Treble"
   show Bass = "Bass"

------ main functions ------

-- keyChords takes a Pattern of Notes indicating tonics,
-- a Pattern of Strings indicating modes,
-- and a Pattern of DegChords
-- and returns a Pattern ValueMap
keyChords :: (Pattern t) => t Note -> t String -> t DegChord -> t ValueMap 
keyChords tonP modeP chordP = note $ patKeyDCToNotes keyP chordP
                              where
                                 keyP = patKey tonP modeP 

-- patKey takes a Pattern of Notes indicating tonics
-- and a Pattern of Strings indicating modes
-- and returns a Pattern of Keys
patKey :: (Pattern t) => t Note -> t String -> t Key
patKey tonP modeP = (strKey <$> tonP) <*> modeP

-- patKeyDCToNotes takes a Pattern of Keys to pass to
-- patDegChordToNotes
patKeyDCToNotes :: (Pattern t) => t Key -> t DegChord -> t Note
patKeyDCToNotes keyP chordP = uncollect $
                              (degChordToNoteList <$> keyP) <*> chordP 

-- degChordToNoteList takes a DegChord and converts it to a list of Notes
-- representing the appropriate chord in the given Key
-- sorts the Notes lowest to highest
degChordToNoteList :: Key -> DegChord -> [Note]
degChordToNoteList key chord = sort $ map (sclDegGetNote key) (degList chord)

-- sclDegGetNote takes a Key key and a Degree sclDeg
-- and returns the Note corresponding to that scale degree in that key
-- note that it pays attention to octaves,
-- so if the key is c4 major, and you ask for the 8th scale degree
-- sclDegGetNote will return c5
-- sclDegGetNote will not return Notes that are too low or high to have
-- a MIDI number, and will instead bring them up or down octaves until
-- they are within MIDI playable range
sclDegGetNote :: Key -> Degree -> Note
sclDegGetNote (Key t mo) (Degree d st) = midiPlayable $ Note 
                                         $ ton + mo!!ind + 12*oct + sem
   where
      ton = unNote t
      sem = fromIntegral st
      len = length mo
      -- (d - 1) because lists are indexed from 0,
      -- but scale degree is traditionally indexed
      -- from 1
      ind = (d - 1) `mod` len -- index in the mode
      oct = fromIntegral $ ((d - 1) `div` len) -- octave adjustment

-- midiPlayable takes a Note and
-- if the note is within MIDI playable range, returns it
-- and if it is not, moves it up or down octaves until it is
midiPlayable :: Note -> Note
midiPlayable i
   | i < (-72) = midiPlayable (i + 12) -- the note is too low, raise it an
                                       -- octave and try again
   | i > 55    = midiPlayable (i - 12) -- the note is too high, lower it an
                                       -- octave and try again
   | otherwise = i -- the note is MIDI playable, return it

-- semiFromInterval takes a Char and an Int representing an interval
-- (i.e., 'm' and 3 to represent a minor third)
-- and returns the number of semitones in that interval
semiFromInterval :: Char -> Int -> Int
semiFromInterval name num
   = (semiFromInterMod7 name numClass) + 12*numOcts 
     where
        (numOcts, numClass) = divMod num 7

-- semiFromInterMod7 takes a Char and an Int representing an interval
-- (here, the Int must be between 0 and 6 inclusive)
-- and returns the number of semitones in that interval
-- note that 0 represents a 7th, but an octave down
-- as divMod 7 7 = (1, 0) (see semiFromInterval)
semiFromInterMod7 :: Char -> Int -> Int
semiFromInterMod7 'm'  nC = case nC of
                               2 -> 1
                               3 -> 3
                               6 -> 8
                               0 -> 10-12
                               _ -> error ("unknown interval m" ++ show nC)
semiFromInterMod7 'M'  nC = case nC of
                               2 -> 2
                               3 -> 4
                               6 -> 9
                               0 -> 11-12
                               _ -> error ("unknown interval M" ++ show nC)
semiFromInterMod7 'd'  nC = case nC of
                               3 -> 2
                               4 -> 4
                               5 -> 6
                               6 -> 7
                               0 -> 9-12
                               _ -> error ("unknown interval d" ++ show nC)
semiFromInterMod7 'A'  nC = case nC of
                               2 -> 3
                               3 -> 5
                               4 -> 6
                               5 -> 8
                               6 -> 10
                               _ -> error ("unknown interval A" ++ show nC)
semiFromInterMod7 'P'  nC = case nC of
                               1 -> 0
                               4 -> 5
                               5 -> 7
                               _ -> error ("unknown interval P" ++ show nC)
semiFromInterMod7 name nC = error ("unknown interval " ++ [name] ++ show nC)


------ modifier functions ------

-- invertDChord takes the first entry in the degList of a DegChord,
-- raises it an octave, and moves it to the end of the degList
invertDChord :: DegChord -> DegChord
invertDChord (DegChord r dL) = DegChord (f r) (firstToLast $ fmap f dL)
   where f = listCompUp dL

-- listCompUp takes a list of Degrees and applies compUp with the first item
-- in the list as the first Degree passed to compUp
listCompUp :: [Degree] -> Degree -> Degree
listCompUp [] = id
listCompUp (x:xs) = compUp x

-- compUp takes two Degrees. If they are the same, it returns the same
-- Degree raised an octave.
-- If they are not the same, it returns the second Degree
compUp :: Degree -> Degree -> Degree
compUp x y
   | x == y    = y `octAdd` 1
   | otherwise = y

-- firstToLast takes a list and moves the first element of the list to
-- the end of the list
firstToLast :: [a] -> [a]
firstToLast [] = []
firstToLast (x:xs) = xs ++ [x]

-- openDChord takes a DegChord and spreads the entries in the degList
-- further from each other
openDChord :: DegChord -> DegChord
openDChord (DegChord r dL) = DegChord (lowestMatch r newDL) newDL
   where newDL = spreadDegList dL

-- lowestMatch finds the lowest Degree in the given list that is the same 
-- scale degree as the given reference Degree
lowestMatch :: Degree -> [Degree] -> Degree
lowestMatch ref dL = (minima $ matches ref dL)!!0 -- Degrees are totally ordered
                                                  -- within the same deg

-- matches finds all Degrees in the given list that are the same scale
-- degree as the given reference Degree
matches :: Degree -> [Degree] -> [Degree]
matches ref dL = filter (octEquiv ref) dL

-- spreadDegList takes a list of Degrees that is assumed to be sorted
-- and spreads the entries further from each other
-- by lowering the lowest third of the list by 2 octaves, 
-- lowering the middle third of the list by 1 octave,
-- and raising the highest note an octave
spreadDegList :: [Degree] -> [Degree]
spreadDegList dL = (map (flip octAdd (-2)) $ parts!!0)
                   ++ (map (flip octAdd (-1)) $ parts!!1)
                   ++ (raiseLast $ parts!!2)
   where parts = dL `splitInN` 3

-- splitInN splits a the given list into N roughly even parts and returns
-- a list of these parts (which are lists)
splitInN :: [a] -> Int -> [[a]]
splitInN [] _ = []
splitInN xs i = (grabFirstN xs lenFirst):(splitInN (remFirstN xs lenFirst) (i-1))
   where lenFirst = (length xs) `div` i

-- grabFirstN takes the first N elements of a list and returns them as a list
grabFirstN :: [a] -> Int -> [a]
grabFirstN []     _ = []
grabFirstN _      0 = []
grabFirstN (x:xs) i = x:(grabFirstN xs (i-1))

-- remFirstN removes the first N elements of a list
remFirstN :: [a] -> Int -> [a]
remFirstN []     _ = []
remFirstN xs     0 = xs
remFirstN (x:xs) i = remFirstN xs (i-1)

-- raiseLast raises the last Degree in a list by an octave
raiseLast :: [Degree] -> [Degree]
raiseLast [] = []
raiseLast dL = (init dL) ++ [(last dL) `octAdd` 1]

-- powerDChord removes all instances of the scale degree 2 higher than
-- the root (the "third") from the degList of the given DegChord
powerDChord :: DegChord -> DegChord
powerDChord (DegChord r dL) 
   = DegChord r (filter (notThird r) dL)

-- notThird takes a reference Degree deg1 and another Degree deg2
-- and returns True if deg2 is not 2 scale degrees higher than deg1
-- (ignoring octaves)
notThird :: Degree -> Degree -> Bool
notThird d1 d2 = not $ isThird d1 d2

-- isThird takes a reference Degree deg1 and another Degree deg2
-- and returns True if deg2 is 2 scale degrees higher than deg1
-- (ignoring octaves)
isThird :: Degree -> Degree -> Bool
isThird (Degree d1 s1) deg2
   = (Degree (d1+2) s1) `octEquiv` deg2

-- degChordUp raises all notes in a DegChord by an octave
degChordUp :: DegChord -> DegChord
degChordUp (DegChord r dL) = DegChord r (map (flip octAdd 1) dL)

-- degChordDown lowers all notes in a DegChord by an octave
degChordDown :: DegChord -> DegChord
degChordDown (DegChord r dL) = DegChord r (map (flip octAdd (-1)) dL)

-- degChordAdd adds a new note to a DegChord by scale degree
-- and a semitone adjustment
-- if the given scale degree is Nothing, it defaults to the root
-- of the given chord
-- the new note is added to the octave above the root
degChordAdd :: Maybe Int -> Int -> DegChord -> DegChord
degChordAdd i st (DegChord r dL) 
   | i == Nothing = DegChord r (degListAddInt st r dL)
   | otherwise    = DegChord r (degListAddSD (fromJust i) st r dL)

-- degListAddInt takes an Int representing number of semitones above the
-- given Degree to add to the given list of Degrees
degListAddInt :: Int -> Degree -> [Degree] -> [Degree]
degListAddInt st (Degree d1 s1) dL
   = topSort $ dL ++ [(Degree d1 (s1+st))]

-- degListAddSD takes two Ints representing a scale degree and
-- a semitone adjustment to add to the given list of Degrees
-- the new Degree will be added above the given reference Degree
-- (expect that the reference Degree is the root of a DegChord)
degListAddSD :: Int -> Int -> Degree -> [Degree] -> [Degree]
degListAddSD i st refDeg dL
   = topSort $ dL ++ [(Degree i st) `makeGT` refDeg] 

-- degChordAddBass adds a new note to a DegChord by scale degree
-- and a semitone adjustment
-- if the given scale degree is Nothing, it defaults to the root
-- of the given chord
-- the new note is added to the octave below the first note in the 
-- degList, which should be the lowest note
degChordAddBass :: Maybe Int -> Int -> DegChord -> DegChord
degChordAddBass i st (DegChord r dL)
   | i == Nothing = DegChord r (degListAddBInt st r dL)
   | otherwise    = DegChord r (degListAddBSD (fromJust i) st dL)

-- degListAddBInt takes an Int representing number of semitones above the
-- given Degree to add to the bass of the given list of Degrees
degListAddBInt :: Int -> Degree -> [Degree] -> [Degree]
degListAddBInt st (Degree d1 s1) dL
   = [(Degree d1 (s1+st)) `makeLT` (dL!!0)] ++ dL

-- degListAddBSD takes two Ints representing a scale degree
-- and a semitone adjustment to add to the bottom of the given
-- list of Degrees
degListAddBSD :: Int -> Int -> [Degree] -> [Degree]
degListAddBSD i st dL
   = [(Degree i st) `makeLT` (dL!!0)] ++ dL
      
-- makeGT takes two Degrees deg1 and deg2 and moves deg1 by octaves
-- until deg1 `partGT` deg2 is True
makeGT :: Degree -> Degree -> Degree
makeGT deg1 deg2
   | deg1 `partGT` deg2 = deg1
   | otherwise          = makeGT (deg1 `octAdd` 1) deg2

-- makeLT takes two Degrees deg1 and deg2 and moves deg1 by octaves
-- until deg1 `partLT` deg2 is True
makeLT :: Degree -> Degree -> Degree
makeLT deg1 deg2
   | deg1 `partLT` deg2 = deg1
   | otherwise          = makeLT (deg1 `octAdd` (-1)) deg2


------ functions that interface with the parser ------

-- degChordToPatSeq takes a Pattern of numbers representing scale degrees
-- and a list of Patterns of lists of DCMods
-- and returns a Pattern of DegChords which have been modified
degChordToPatSeq :: (Pattern t) => (DegChord -> a) -> t Int -> [t [DCMod]] -> t a 
degChordToPatSeq f degP modsP = do
                                 d <- patNumToDeg degP
                                 let ch = degTriad d
                                 applyDCModPatSeq f (return ch) modsP 

-- patNumToDeg converts a Pattern of Ints to a Pattern of Degrees
patNumToDeg :: (Pattern t) => t Int -> t Degree
patNumToDeg pat = fmap (\x -> Degree {deg = x, semi = 0} ) pat

-- applyDCModPatSeq applies a List of Patterns of Lists of DCMods
-- to a Pattern of DegChords
applyDCModPatSeq :: (Pattern t) => (DegChord -> a) -> t DegChord -> [t [DCMod]] -> t a
applyDCModPatSeq f pat [] = fmap f pat
applyDCModPatSeq f pat (mP:msP) = applyDCModPatSeq f (applyDCModPat pat mP) msP

-- applyDCModPat applies a Pattern of Lists of DCMods
-- to a Pattern of DegChords
applyDCModPat :: (Pattern t) => t DegChord -> t [DCMod] -> t DegChord
applyDCModPat pat modsP = do
                            dch <- pat
                            ms <- modsP 
                            return $ (foldl (flip applyDCMod) dch) ms 

-- applyDCMod applies a DCMod to a DegChord
applyDCMod :: DCMod -> DegChord -> DegChord
applyDCMod DInvert = invertDChord
applyDCMod DOpen = openDChord
applyDCMod DPower = powerDChord
applyDCMod DUp = degChordUp
applyDCMod DDown = degChordDown
applyDCMod (DAdd Treble i st) = degChordAdd i st
applyDCMod (DAdd Bass i st) = degChordAddBass i st
