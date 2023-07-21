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
import qualified Data.PartialOrd     as PO
import           Sound.Tidal.Types
import           Sound.Tidal.Scales
import           Sound.Tidal.Pattern
import           Sound.Tidal.Params

------ types, classes, and related functions ------

-- Degree type
-- for indicating scale degree when you don't know what scale you are using
-- includes the degree, a semitone adjustment, and  octave adjustment
data Degree = Degree { deg :: Int
                     , semi :: Int
                     , octs :: Int
                     } deriving (Show)

-- degAdd function to easily add to the degree field of a Degree
degAdd :: Degree -> Int -> Degree
degAdd d i = Degree {deg = newDeg, semi = newSemi, octs = newOcts}
             where
                newDeg = deg d + i
                newSemi = semi d
                newOcts = octs d

-- octAdd function to easily add to the octaves field of a Degree
octAdd :: Degree -> Int -> Degree
octAdd d i = Degree {deg = newDeg, semi = newSemi, octs = newOcts}
             where
                newDeg = deg d
                newSemi = semi d
                newOcts = octs d + i

-- semiAdd function to easily add to the octaves field of a Degree
semiAdd :: Degree -> Int -> Degree
semiAdd d i = Degree {deg = newDeg, semi = newSemi, octs = newOcts}
              where
                 newDeg = deg d
                 newSemi = semi d + i
                 newOcts = octs d 


-- degrees can be compared
-- 12 semitones = 1 octave
instance Eq Degree where 
   (Degree d1 s1 o1) == (Degree d2 s2 o2) 
      = (d1 == d2) && (s1 + 12*o1 == s2 + 12*o2)

instance PO.PartialOrd Degree where
   (Degree d1 s1 o1) <= (Degree d2 s2 o2) 
      =  (d1 <= d2) && (s1 + 12*o1 == s2 + 12*o2)
      || (d1 == d2) && (s1 + 12*o1 <= s2 + 12*o2)

-- isTopSorted checks if a list of partially ordered elements
-- is in a valid topological sorting
isTopSorted :: (PO.PartialOrd a) => [a] -> Bool
isTopSorted [] = True
isTopSorted (x:xs) = if length xs == 0
                        then True
                        else sorted x (xs!!0) && isTopSorted xs
                     where
                        sorted a b
                           | a PO.<= b                 = True
                           | PO.compare a b == Nothing = True
                           | otherwise                 = False

-- topSort returns a valid topological sorting of a list of
-- partially ordered elements
topSort :: (PO.PartialOrd a) => [a] -> [a]
topSort [] = []
topSort list = mins ++ (topSort remaining)
               where
                  mins = PO.minima list
                  remaining = filter (flip PO.notElem mins) list

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
strKey ton modeStr = if jMode /= Nothing -- make sure the scale is findable
                         then Key {tonic = ton, mode = (fromJust jMode)}
                         else error "Mode not available"
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
sclDegGetNote k sclDeg = midiPlayable $ Note $ ton + (mode k)!!ind + 12*oct
                           where
                              ton = unNote $ tonic k
                              len = length $ mode k
                              d = deg sclDeg
                              -- (deg - 1) because lists are indexed from 0,
                              -- but scale degree is traditionally indexed
                              -- from 1
                              oct = fromIntegral $ ((d - 1) `div` len)
                                       + (octs sclDeg) -- octave adjustment
                              ind = (d - 1) `mod` len -- index in the mode

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
invertDChord (DegChord r [])                = DegChord r []
invertDChord (DegChord r (first:remaining)) = DegChord newR dL
   where
       -- grab the first degree and 
       -- put the remaining degrees in a separate list
       firstUp = first `octAdd` 1
       dL = remaining ++ [firstUp]
       -- the root stays the same unless it was the one that
       -- was raised an octave
       newR = if first == r 
                 then firstUp
                 else r 

-- openDChord takes a DegChord and spreads the entries in the degList
-- further from each other
openDChord :: DegChord -> DegChord
openDChord (DegChord r [])  = DegChord r []
openDChord (DegChord r [d]) = DegChord r [d]
openDChord (DegChord r ds)  = DegChord newR dL
   where
      len = length ds 
      -- split the list roughly in half with a single
      -- element in the center
      splitInd = len `quot` 2
      (down, stay:raise) = splitAt splitInd ds 
      dL = (spreadDown down) ++ [stay] ++ (spreadUp raise)
      -- grab the deg from the original root
      rDeg = deg r 
      -- grab all instances of the root deg in dL
      roots = filter (\x -> deg x == rDeg) dL
      -- grab the lowest to set to the new root
      -- (even though Degrees are partially ordered,
      -- within the same deg they are totally ordered)
      newR = (PO.minima roots)!!0

-- spreadDown takes a List of Degrees and spreads them out
-- by lowering them by octaves
-- entries earlier in the list are lowered more
spreadDown :: [Degree] -> [Degree]
spreadDown [] = []
spreadDown degs = (spreadDown (init degs)) ++ [(last degs) `octAdd` (-1)]

-- spreadUp takes a List of Degrees and spreads them out
-- by raising them by octaves
-- entries later in the list are raised more
spreadUp :: [Degree] -> [Degree]
spreadUp [] = []
spreadUp degs = [(head degs) `octAdd` 1] ++ (spreadUp (tail degs))

-- powerDChord removes all instances of the scale degree 2 higher than
-- the root (the "third") from the degList of the given DegChord
powerDChord :: DegChord -> DegChord
powerDChord chord = DegChord {degRoot = r, degList = dL}
                    where
                       r = degRoot chord
                       dr = deg r
                       dL = filter (\x -> deg x /= dr + 2) (degList chord)

-- degChordUp raises all notes in a DegChord by an octave
degChordUp :: DegChord -> DegChord
degChordUp chord = DegChord {degRoot = r, degList = dL}
                   where
                      r = degRoot chord
                      dL = map (flip octAdd 1) (degList chord)

-- degChordDown lowers all notes in a DegChord by an octave
degChordDown :: DegChord -> DegChord
degChordDown chord = DegChord {degRoot = r, degList = dL}
                     where
                        r = degRoot chord
                        dL = map (flip octAdd (-1)) (degList chord)

-- degChordAdd adds a new note to a DegChord by scale degree
-- and a semitone adjustment
-- if the given scale degree is Nothing, it defaults to the root
-- of the given chord
-- the new note is added to the octave above the root
degChordAdd :: Maybe Int -> Int -> DegChord -> DegChord
degChordAdd i st chord = if i == Nothing
                            then degChordAddInt st chord
                            else degChordAddSD (fromJust i) st chord

degChordAddInt :: Int -> DegChord -> DegChord
degChordAddInt st chord = DegChord {degRoot = r, degList = dL}
                          where
                          r = degRoot chord
                          rootDeg = deg r
                          rootOct = octs r
                          newDeg = Degree { deg = rootDeg
                                          , semi = st
                                          , octs = rootOct } 
                          newDL = degList chord ++ [newDeg]
                          dL = if isTopSorted newDL
                                  then newDL
                                  else topSort newDL

degChordAddSD :: Int -> Int -> DegChord -> DegChord
degChordAddSD i st chord = DegChord {degRoot = r, degList = dL}
                           where
                              r = degRoot chord
                              rootDeg = deg r
                              rootOct = octs r
                              newDeg  
                                 | i <= rootDeg 
                                    = Degree { deg = i
                                             , semi = st
                                             , octs = rootOct + 1 }
                                 | otherwise -- (i > rootDeg)  
                                    = Degree { deg = i
                                             , semi = st
                                             , octs = rootOct } 
                              newDL = degList chord ++ [newDeg]
                              dL = if isTopSorted newDL
                                      then newDL
                                      else topSort newDL

-- degChordAddBass adds a new note to a DegChord by scale degree
-- and a semitone adjustment
-- if the given scale degree is Nothing, it defaults to the root
-- of the given chord
-- the new note is added to the octave below the first note in the 
-- degList, which should be the lowest note
degChordAddBass :: Maybe Int -> Int -> DegChord -> DegChord
degChordAddBass i st chord = if i == Nothing
                                then degChordAddBInt st chord
                                else degChordAddBSD (fromJust i) st chord

degChordAddBInt :: Int -> DegChord -> DegChord
degChordAddBInt st chord = DegChord {degRoot = r, degList = dL}
                           where
                              r = degRoot chord
                              rootDeg = deg r
                              lowest = (degList chord)!!0
                              lOct = octs lowest
                              newDeg = Degree { deg = rootDeg
                                              , semi = st
                                              , octs = lOct - 1}
                              -- do not need to sort, as this should be the
                              -- new lowest note
                              dL = newDeg:(degList chord)

degChordAddBSD :: Int -> Int -> DegChord -> DegChord
degChordAddBSD i st chord = DegChord {degRoot = r, degList = dL}
                            where
                               r = degRoot chord
                               lowest = (degList chord)!!0
                               lDeg = deg lowest
                               lOct = octs lowest
                               newDeg
                                  | i < lDeg
                                     = Degree { deg = i
                                              , semi = st
                                              , octs = lOct }
                                  | otherwise -- (i >= lDeg)
                                     = Degree { deg = i
                                              , semi = st
                                              , octs = lOct - 1 }
                               -- do not need to sort, as this should be the
                               -- new lowest note
                               dL = newDeg:(degList chord)


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
patNumToDeg pat = fmap (\x -> Degree {deg = x, semi = 0, octs = 0}) pat

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
