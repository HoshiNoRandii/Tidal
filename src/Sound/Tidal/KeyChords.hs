{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Sound.Tidal.KeyChords where

{-
   This is HoshiNoRandii's module to implement chords in TidalCycles in
   a way that makes it easier to stay in a desired key without knowing
   a lot of music theory.
-}

import Control.Applicative ( Applicative(liftA2) )
import Data.List           ( sort )
import Data.Maybe          ( fromJust )
import Sound.Tidal.Params  ( note )
import Sound.Tidal.Pattern ( Pattern(uncollect) )
import Sound.Tidal.Scales  ( scaleTable )
import Sound.Tidal.Types   ( Note(..), ValueMap )


------ types, classes, and related functions ------

-- GenChord type
-- a GenChord stores a scale degree to build a chord from
-- as well as chord modifiers
data GenChord = GenChord { sclDeg :: Int
                         , mods :: [NCMod]
                         } deriving (Show)

-- NoteChord type
-- a NoteChord stores a list of Notes
-- as well as the root of the chord and the Key
-- the noteList should be in ascending order
data NoteChord = NoteChord { noteList :: [Note]
                           , root :: Note
                           , inKey :: Key
                           } deriving (Show)

-- InRange type
data InRange = Yes | High | Low | No deriving (Eq, Show)

-- irAnd is defining the logical 'and' operator for InRange values
irAnd :: InRange -> InRange -> InRange
irAnd High Low = No
irAnd No _ = No
irAnd Yes x = x
irAnd x y
  | x == y    = x
  | otherwise = irAnd y x

-- Move type
-- for tracking whether a chord has moved up or down
data Move = Up | Down deriving Eq

-- NCMod type
-- modifiers for NoteChords
data NCMod = NInvert | NOpen | NPower | NUp | NDown
           | NAdd AddWhere (Maybe Int) Int deriving (Eq)

instance Show NCMod where
  show NInvert = "NoteChord Invert"
  show NOpen = "NoteChord Open"
  show NPower = "Power NoteChord"
  show NUp = "NoteChord Octave Up"
  show NDown = "NoteChord Octave Down"
  show (NAdd x i st) = "NoteChord Add Scale Degree" ++ show i ++ "+" ++ show st ++ "semitones to" ++ show x

-- AddWhere type
-- to indicate adding tones to the treble or bass of a chord
data AddWhere = Treble | Bass deriving Eq

instance Show AddWhere where
  show Treble = "Treble"
  show Bass = "Bass"

-- Key type
-- includes the tonic and the "scale" (mode)
data Key = Key { tonic :: Note
               , mode :: [Double]
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

-- rfMod is a version of the mod function that takes
-- RealFrac inputs
-- for integral inputs, x `rfMod` y == x `mod` y
rfMod :: (RealFrac a, Eq a) => a -> a -> a
rfMod x y
  | y == 0    = error "division by 0"
  | otherwise = x - (div * y)
  where div = fromIntegral $ floor $ x/y

------ main functions ------

-- genToNoteChord takes a GenChord and a Key and returns
-- the corresponding NoteChord, whose noteList corresponds to
-- the triad build on the sclDeg in the given Key, with
-- the modifications applied
-- the noteList should be sorted in ascending order
genToNoteChord :: GenChord -> Key -> NoteChord
genToNoteChord (GenChord sd ms) key
  = (flip applyNCMods) ms $
  NoteChord (sclDegKeyTriad sd key) (sclDegGetNote sd key) key

-- sclDegKeyTriad takes an Int representing a scale degree
-- and a Key and returns a list of three Notes corresponding
-- to a triad built on that scale degree in the given Key
sclDegKeyTriad :: Int -> Key -> [Note]
sclDegKeyTriad sd key =
  (sclDegGetNote sd key):(sclDegGetNote (sd+2) key):(sclDegGetNote (sd+4) key):[]

-- sclDegGetNote takes an Int representing scale degree
-- and a Key and returns the Note corresponding to that
-- scale degree in that Key.
-- Note that it pays attention to octaves, so for instance,
-- sclDegKeyTriad 8 (strKey c4 "major") == c5.
sclDegGetNote :: Int -> Key -> Note
sclDegGetNote sd (Key t mo) = Note $ ton + mo!!ind + 12*oct
  where
    ton = unNote t
    len = length mo
    -- (sd-1) because lists are indexed from 0,
    -- but scale degree is traditionally indexed from 1
    ind = (sd-1) `mod` len -- index in the mode
    oct = fromIntegral $ ((sd-1) `div` len) -- octave adjust

-- noteGetSclDeg takes a Note and a Key and returns an Int
-- representing the scale degree of that Note in that Key.
-- it does not pay attention to octaves, so for instance,
-- if key = strKey 0 "major",
-- then noteGetSclDeg 0 key == noteGetSclDeg 12 == 1
noteGetSclDeg :: Note -> Key -> Int
noteGetSclDeg n (Key t mo)
  | degs == [] = error ("note " ++ (show n) ++ " not in key")
  | otherwise  = (degs!!0)+1 -- +1 because lists are indexed from 0,
                             -- and scale degree is indexed from 1
  where toC = n-t
        resClass = unNote $ toC `rfMod` 12
        indList = [0..((length mo)-1)]
        test x = mo!!x == resClass
        degs = filter test indList

-- noteMidiPlayable takes a Note and
-- if the note is within MIDI playable range, returns it
-- and if it is not, moves it up or down octaves until it is
noteMidiPlayable :: Note -> Note
noteMidiPlayable i
   | i < (-72) = noteMidiPlayable (i + 12)
   | i > 55    = noteMidiPlayable (i - 12)
   | otherwise = i

-- isMidiPlayable checks if a Note is within MIDI playable range
isMidiPlayable :: Note -> InRange
isMidiPlayable i
  | i < (-72) = Low
  | i > 55    = High
  | otherwise = Yes

-- listIsMidiPlayable checks if a list of Notes are all within
-- MIDI playable range
listIsMidiPlayable :: [Note] -> InRange
listIsMidiPlayable ns = foldr f Yes ns
  where f n ir = (isMidiPlayable n) `irAnd` ir

-- TODO: make sure this is applied before the chord is played
-- nChordMidiPlayable takes a NoteChord and if the whole noteList
-- is within MIDI playable range, returns it,
-- and if it is not, moves the entire chord up or down octaves until
-- it is. This includes changing the root and key.
-- if it is not possible for the entire noteList to be in MIDI
-- playable range by moving by octaves, an error is thrown
nChordMidiPlayable :: NoteChord -> NoteChord
nChordMidiPlayable nc = nChordMidiPlayable' Yes nc

nChordMidiPlayable' :: InRange -> NoteChord -> NoteChord
nChordMidiPlayable' No _
  = error "chord is too wide for MIDI playable range"
nChordMidiPlayable' prevIR (NoteChord nL r (Key ton m))
  | inRange == High
    = nChordMidiPlayable' (prevIR `irAnd` inRange) $
      NoteChord (noteListDown nL) (r-12) (Key (ton-12) m)
  | inRange == Low
    = nChordMidiPlayable' (prevIR `irAnd` inRange) $
      NoteChord (noteListUp nL) (r+12) (Key (ton+12) m)
  | otherwise
    = NoteChord nL r (Key ton m)
  where inRange = listIsMidiPlayable nL

-- applyNCMods takes a NoteChord and a list of NCMods
-- and applies the NCMods to the noteList in the order
-- they are listed
applyNCMods :: NoteChord -> [NCMod] -> NoteChord
applyNCMods nc [] = nc
applyNCMods nc (m:ms) = applyNCMods (applyNCMod nc m) ms

-- TODO: implement this lmao
-- applyNCMod takes a NoteChord and an NCMod
-- and applies the NCMod to the noteList
applyNCMod :: NoteChord -> NCMod -> NoteChord
applyNCMod nc m = nc


------ modifier functions ------

-- invertNoteChord takes the bottom Note in a NoteChord
-- and raises it an octave.
-- The root of the NoteChord will be updated if it is raised
invertNoteChord :: NoteChord -> NoteChord
invertNoteChord (NoteChord nL r key)
  = NoteChord (sort $ fmap f nL) (f r) key
  where f = listFirstCompUp nL

-- listFirstCompUp takes a list of numbers and applies compUp
-- with the first item in the list as the first number passed
-- to compUp
listFirstCompUp :: (Num a, Eq a) => [a] -> a -> a
listFirstCompUp [] = id
listFirstCompUp (x:_) = compUp x

-- compUp takes two numbers. If they are the same, it returns
-- that number +12.
-- if they are not the same, it returns the second number.
compUp :: (Num a, Eq a) => a -> a -> a
compUp x y
  | x == y    = y+12
  | otherwise = y

-- openNoteChord takes a NoteChord and spreads out the entries
-- in the noteList by lowering every other Note (except the
-- last one) by an octave
-- the root of the NoteChord will be updated if it is lowered
openNoteChord :: NoteChord -> NoteChord
openNoteChord (NoteChord nL r key)
  = NoteChord (sort $ fmap f nL) (f r) key
  where f = listCompDown (everyOtherButLast nL)

-- listCompDown takes a list of numbers and a number x
-- if x is in the list, it returns x-12
-- otherwise it returns x
listCompDown :: (Num a, Eq a) => [a] -> a -> a
listCompDown list x
  | x `elem` list = x-12
  | otherwise     = x

-- everyOtherButLast takes a list and returns the list
-- containing every other element (except the last one)
everyOtherButLast :: [a] -> [a]
everyOtherButLast [] = []
everyOtherButLast (_:[]) = []
everyOtherButLast (x:xs) = x:(everyOtherButLast (tail xs))

-- powerNoteChord removes all Notes from a NoteChord that are not
-- (up to octaves) the root or the "fifth"
-- here, the "fifth" is the Note 4 scale degrees above the root
-- in a western seven-note scale, this produces a "power chord,"
-- hence the name
powerNoteChord :: NoteChord -> NoteChord
powerNoteChord (NoteChord nL r k)
  = NoteChord (filter test nL) r k
  where test = inPower (NoteChord nL r k)

-- inPower checks if the given Note should be in the power chord
-- version of the given NoteChord
inPower :: NoteChord -> Note -> Bool
inPower nC n = (isRoot nC n) || (isFifth nC n)

-- isRoot checks if the given Note is the root (or an octave
-- adusted version of it) of the given NoteChord
isRoot :: NoteChord -> Note -> Bool
isRoot (NoteChord nL r k) n = (n `rfMod` 12) == (r `rfMod` 12)

-- isFifth checks if the given Note is the "fifth" (or an octave
-- adjusted version of it) of the given NoteChord
-- here the "fifth" is the Note 4 scale degrees above the root
isFifth :: NoteChord -> Note -> Bool
isFifth (NoteChord nL r k) n
  = (n `rfMod` 12) == (fifth `rfMod` 12)
  where fifth = sclDegGetNote (rootDeg+4) k
        rootDeg = noteGetSclDeg r k

-- noteChordUp raises the given NoteChord by an octave
noteChordUp :: NoteChord -> NoteChord
noteChordUp (NoteChord nL r key)
  = NoteChord (noteListUp nL) (r+12) key

-- noteListUp raises all Notes in a list by an octave
noteListUp :: [Note] -> [Note]
noteListUp ns = map (+12) ns

-- noteChordDown lowers the given NoteChord by an octave
noteChordDown :: NoteChord -> NoteChord
noteChordDown (NoteChord nL r key)
  = NoteChord (noteListDown nL) (r-12) key

-- noteListDown lowers all Notes in a list by an octave
noteListDown :: [Note] -> [Note]
noteListDown ns = map (+(-12)) ns

-- noteChordAdd adds a new note to a NoteChord by scale degree
-- and a semitone adjustment
-- if the given scale degree is Nothing, it defaults to the root
-- of the given chord
-- the new note is added above the root
noteChordAdd :: Maybe Int -> Int -> NoteChord -> NoteChord
noteChordAdd d st (NoteChord nL r key)
  = NoteChord (noteListAddNote toAdd nL) r key
  where
    s = fromIntegral st
    toAdd
      | d == Nothing
        = r+s
      | otherwise
        = ((sclDegGetNote (fromJust d) key)+s) `noteHigherThan` r

-- noteChordAddBass adds a new note to a note to the bass of
-- a NoteChord by scale degree and a semitone adjustment
-- if the given scale degree is Nothing, it defaults to the
-- root of the given chord
-- the new note is added at the bottom of the noteList
-- assumes the noteList is sorted
noteChordAddBass :: Maybe Int -> Int -> NoteChord -> NoteChord
noteChordAddBass d st (NoteChord nL r key)
  = NoteChord (noteListAddNote toAdd nL) r key
  where
    s = fromIntegral st
    toAdd
      | d == Nothing
        = (r+s) `noteLowerThan` (nL!!0)
      | otherwise
        = ((sclDegGetNote (fromJust d) key)+s)
          `noteLowerThan` (nL!!0)

-- noteListAddNote takes a Note to add to a list of Notes
-- the list will remain sorted
noteListAddNote :: Note -> [Note] -> [Note]
noteListAddNote n nL = sort $ n:nL

-- noteHigherThan takes a Note n and a reference Note r
-- and makes sure n is higher than r, moving it by octaves
noteHigherThan :: Note -> Note -> Note
noteHigherThan n r
  | n > r     = n
  | otherwise = noteHigherThan (n+12) r

-- noteLowerThan takes a Note n and a reference Note r
-- and makes sure n is lower than r, moving it by octaves
noteLowerThan :: Note -> Note -> Note
noteLowerThan n r
  | n < r     = n
  | otherwise = noteLowerThan (n-12) r
