module Sound.Tidal.KeyChords where

{-
   This is HoshiNoRandii's module to implement chords in TidalCycles in
   a way that makes it easier to stay in a desired key without knowing
   a lot of music theory.
-}

import Data.Maybe
import Sound.Tidal.Types
import Sound.Tidal.Scales
import Sound.Tidal.Pattern

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

-- DegChord type
-- a chord of scale degrees
-- includes the root of the chord
-- and a list of the other scale degrees in the chord
data DegChord = DegChord { degRoot :: Degree
                         , degList :: [Degree]
                         } deriving (Show)

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

-- KeyChord type
-- includes the key,
-- the root of the chord and its degree rootDeg,
-- and a list of notes in the chord
-- entries in noteList should be in order of ascending pitch
data KeyChord = KeyChord { key :: Key
                         , rootDeg :: Degree
                         , root :: Note
                         , noteList :: [Note]
                         } deriving (Show)


-- DCMod type
-- modifiers for DegChords
data DCMod = DInvert

instance Show DCMod where
   show DInvert = "DegChord Invert"


------ main functions ------

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
                            
-- sclDegGetNote takes a Key key and a Degree sclDeg
-- and returns the Note corresponding to that scale degree in that key
-- note that it pays attention to octaves,
-- so if the key is c4 major, and you ask for the 8th scale degree
-- sclDegGetNote will return c5
sclDegGetNote :: Key -> Degree -> Note
sclDegGetNote k sclDeg = Note $ (mode k)!!ind + 12*oct
                           where
                              len = length $ mode k
                              d = deg sclDeg
                              -- (deg - 1) because lists are indexed from 0,
                              -- but scale degree is traditionally indexed
                              -- from 1
                              oct = fromIntegral $ ((d - 1) `div` len)
                                       + (octs sclDeg) -- octave adjustment
                              ind = (d - 1) `mod` len -- index in the mode

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

------ functions that interface with the parser ------

-- degChordToPatSeq takes a Pattern of numbers representing scale degrees
-- and a list of Patterns of lists of DCMods
-- and returns a Pattern of DegChords which have been modified
degChordToPatSeq :: (Num a, Enum a, Integral a, Pattern t) => t a -> [t [DCMod]] -> t DegChord 
degChordToPatSeq degP modsP = do
                                 d <- patNumToDeg degP
                                 let ch = degTriad d
                                 applyDCModPatSeq (return ch) modsP 

-- patNumToDeg converts a Pattern of numbers to a Pattern of Degrees
patNumToDeg :: (Num a, Enum a, Integral a, Pattern t) => t a -> t Degree
patNumToDeg pat = fmap ((\x -> Degree {deg = x, octs = 0}).fromIntegral) pat

-- applyDCModPatSeq applies a List of Patterns of Lists of DCMods
-- to a Pattern of DegChords
applyDCModPatSeq :: (Pattern t) => t DegChord -> [t [DCMod]] -> t DegChord
applyDCModPatSeq pat [] = pat
applyDCModPatSeq pat (mP:msP) = applyDCModPatSeq (applyDCModPat pat mP) msP

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
