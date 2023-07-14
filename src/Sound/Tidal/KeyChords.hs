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
-- includes the degree and an octave adjustment
data Degree = Degree { deg :: Int
                     , octs :: Int
                     } deriving (Show)

-- degAdd function to easily add to the degree field of a Degree
degAdd :: Degree -> Int -> Degree
degAdd d i = Degree {deg = newDeg, octs = newOcts}
             where
                newDeg = deg d + i
                newOcts = octs d

-- octAdd function to easily add to the octaves field of a Degree
octAdd :: Degree -> Int -> Degree
octAdd d i = Degree {deg = newDeg, octs = newOcts}
             where
                newDeg = deg d
                newOcts = octs d + i

-- degrees can be compared
instance Eq Degree where 
   (Degree d1 o1) == (Degree d2 o2) = (d1 == d2) && (o1 == o2)

instance PO.PartialOrd Degree where
   (Degree d1 o1) <= (Degree d2 o2) = (d1 == d2) && (o1 <= o2)
   (Degree d1 o1) <= (Degree d2 o2) = (o1 == o2) && (d1 <= d2)

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
data DCMod = DInvert | DOpen deriving Eq

instance Show DCMod where
   show DInvert = "DegChord Invert"
   show DOpen = "DegChord Open"


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
degChordToNoteList :: Key -> DegChord -> [Note]
degChordToNoteList key chord = map (sclDegGetNote key) (degList chord)

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
midiPlayable n
   | (-72) <= n && n <= 55 = n -- the note is MIDI playable, return it
   | n < (-72)             = midiPlayable (n + 12) -- the note is too low,
                                                   -- raise it an octave
                                                   -- and try again
   | n > 55                = midiPlayable (n - 12) -- the note is too high,
                                                   -- lower it an octave
                                                   -- and try again


------ modifier functions ------

-- invertDChord takes the first entry in the degList of a DegChord,
-- raises it an octave, and moves it to the end of the degList
invertDChord :: DegChord -> DegChord
invertDChord chord = DegChord {degRoot = r, degList = dL}
                     where
                        -- grab the first degree and 
                        -- put the remaining degrees in a separate list
                        (first:remaining) = degList chord
                        firstUp = first `octAdd` 1
                        dL = remaining ++ [firstUp]
                        -- the root stays the same unless it was the one that
                        -- was raised an octave
                        r = if first == degRoot chord
                               then firstUp
                               else degRoot chord

-- openDChord takes a DegChord and spreads the entries in the degList
-- further from each other
openDChord :: DegChord -> DegChord
openDChord chord = DegChord {degRoot = r, degList = dL}
                   where
                      oldDL = degList chord
                      len = length oldDL
                      -- split the list roughly in half with a single
                      -- element in the center
                      splitInd = len `quot` 2
                      (down, stay:up) = splitAt splitInd oldDL
                      dL = (spreadDown down) ++ [stay] ++ (spreadUp up)
                      -- grab the deg from the original root
                      rDeg = deg (degRoot chord)
                      -- grab all instances of the root deg in dL
                      roots = filter (\x -> deg x == rDeg) dL
                      -- grab the lowest to set to the new root
                      -- (even though Degrees are partially ordered,
                      -- within the same deg they are totally ordered)
                      r = (PO.minima roots)!!0

-- spreadDown takes a List of Degrees and spreads them out
-- by lowering them by octaves
-- entries earlier in the list are lowered more
spreadDown :: [Degree] -> [Degree]
spreadDown [] = []
spreadDown chords = (spreadDown (init chords)) ++ [(last chords) `octAdd` (-1)]

-- spreadUp takes a List of Degrees and spreads them out
-- by raising them by octaves
-- entries later in the list are raised more
spreadUp :: [Degree] -> [Degree]
spreadUp [] = []
spreadUp chords = [(head chords) `octAdd` 1] ++ (spreadUp (tail chords))


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
patNumToDeg pat = fmap (\x -> Degree {deg = x, octs = 0}) pat

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
