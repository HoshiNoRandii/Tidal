module Sound.Tidal.KeyChords where

{-
   This is HoshiNoRandii's module to implement chords in TidalCycles in
   a way that makes it easier to stay in a desired key without knowing
   a lot of music theory.
-}

import Data.Maybe
import Sound.Tidal.Types
import Sound.Tidal.Scales

-- RChord type
-- includes the root of the chord
-- and a list of notes in the chord
data RChord = RChord { root :: Note
                     , noteList :: [Note]
                     } deriving (Show)

-- RKey type
-- includes the tonic and the "scale" (mode)
data RKey = RKey { tonic :: Note
                 , mode :: [Double] -- TODO: figure out how to have "Fractional a => [a]" here, to match Scales.hs
                 } deriving (Show)

-- strRKey takes a Note ton as the tonic
-- and a String modeStr with the name of a scale (mode)
-- and contructs an RKey
strRKey :: Note -> String -> RKey
strRKey ton modeStr = if jMode /= Nothing -- make sure the scale is findable
                         then RKey {tonic = ton, mode = (fromJust jMode)}
                         else error "Mode not available"
                      where jMode = lookup modeStr scaleTable 

-- sclDegTriad takes an RKey key and an Int sclDeg
-- and returns an RChord which is a triad built in the given key
-- on the given scale degree
sclDegTriad :: RKey -> Int -> RChord
sclDegTriad key sclDeg = RChord {root = r, noteList = nL}
                         where
                            -- find the root
                            r = sclDegGetNote key sclDeg
                            -- find the third and fifth
                            third = sclDegGetNote key (sclDeg + 2)
                            fifth = sclDegGetNote key (sclDeg + 4)
                            -- put the noteList together
                            nL = r:third:fifth:[]

-- sclDegGetNote takes an RKey key and an Int sclDeg
-- and returns the Note corresponding to that scale degree in that key
-- note that it pays attention to octaves,
-- so if the root of key is c4, and you ask for the 8th scale degree
-- sclDegGetNote will return c5
sclDegGetNote :: RKey -> Int -> Note
sclDegGetNote key sclDeg = Note $ (mode key)!!ind + 12*oct
                           where
                              len = length $ mode key
                              -- (sclDeg - 1) because lists are indexed from 0,
                              -- but scale degree is traditionally indexed
                              -- from 1
                              oct = fromIntegral $ (sclDeg - 1) `div` len -- octave adjustment
                              ind = (sclDeg - 1) `mod` len -- index in the mode

-- keyChords takes a Pattern Int and converts it to a Pattern Note that will
-- play the corresponding chords in the specified key.
-- For example,
--    d1 $ n keyChords "a" "major" "1 2 4 5" # s "superfm"
-- will play the I ii IV V chords in the key of A major,
-- i.e. Amaj, Bmin, Dmaj, Emaj
-- Note in particular that it is not necessary to know that the ii chord of a 
-- major key is minor in order to stay in the key.

