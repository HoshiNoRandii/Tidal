module Sound.Tidal.KeyChords where

{-
   This is HoshiNoRandii's module to implement chords in TidalCycles in
   a way that makes it easier to stay in a desired key without knowing
   a lot of music theory.
-}

import Data.Maybe
import Sound.Tidal.Pattern


-- keyChords takes a Pattern Int and converts it to a Pattern Note that will
-- play the corresponding chords in the specified key.
-- For example,
--    d1 $ n keyChords "a" "major" "1 2 4 5" # s "superfm"
-- will play the I ii IV V chords in the key of A major,
-- i.e. Amaj, Bmin, Dmaj, Emaj
-- Note in particular that it is not necessary to know that the ii chord of a 
-- major key is minor in order to stay in the key.
--keyChords :: String -> String -> Pattern Int -> Pattern Note

