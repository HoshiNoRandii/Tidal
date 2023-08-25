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

-- RChord type
-- an RChord stores a scale degree to build a chord from
-- as well as chord modifiers
data RChord = RChord { sclDeg :: Int
                     , mods :: [RCMod]
                     } deriving (Show)

-- RCMod type
-- modifiers for RChords
data RCMod = RInvert | ROpen | RPower | RUp | RDown
           | RAdd AddWhere (Maybe Int) Int deriving Eq

instance Show RCMod where
  show RInvert = "RChord Invert"
  show ROpen = "RChord Open"
  show RPower = "Power RChord"
  show RUp = "RChord Octave Up"
  show RDown = "RChord Octave Down"
  show (RAdd x i st) = "RChord Add Scale Degree" ++ show i ++ "+" ++ show st ++ "semitones to" ++ show x

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


------ main functions ------

-- rChordToNotes takes an RChord and a Key and returns
-- a list of Notes corresponding to the triad built
-- on the scale degree indicated in the RChord in the
-- given Key, with the modifications indicated in the RChord.
-- The Note list should be sorted in ascending order
rChordToNotes :: RChord -> Key -> [Note]
rChordToNotes (RChord sd ms) key =
  (flip applyRMods) ms $ sclDegKeyTriad sd key

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
-- Only returns Notes in a MIDI playable range (will ignore
-- octaves for this purpose).
sclDegGetNote :: Int -> Key -> Note
sclDegGetNote sd (Key t mo) =
  midiPlayable $ Note $ ton + mo!!ind + 12*oct
  where
    ton = unNote t
    len = length mo
    -- (sd-1) because lists are indexed from 0,
    -- but scale degree is traditionally indexed from 1
    ind = (sd-1) `mod` len -- index in the mode
    oct = fromIntegral $ ((sd-1) `div` len) -- octave adjust

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

-- isMidiPlayable checks if a Note is within MIDI playable range
isMidiPlayable :: Note -> Bool
isMidiPlayable i = (i >= (-72)) && (i <= 55)

-- listIsMidiPlayable checks if a list of Notes are all within
-- MIDI playable range
listIsMidiPlayable :: [Note] -> Bool
listIsMidiPlayable ns = foldr f True ns
  where f n b = (isMidiPlayable n) && b

-- applyRMods takes a list of Notes and a list of RCMods
-- and applies the RMods to the list of Notes in the order
-- they are listed
applyRMods :: [Note] -> [RCMod] -> [Note]
applyRMods ns [] = ns
applyRMods ns (x:xs) = applyRMods (applyRMod ns x) xs

-- TODO: implement this lmao
-- applyRMod takes a list of Notes and an RCMod
-- and applies the RMod to the list of Notes
applyRMod :: [Note] -> RCMod -> [Note]
applyRMod ns rm = ns


------ modifier functions ------

-- invertChord takes the first Note in a list, raises it an octave,
-- and moves it to the end of the list
-- If this operation would take a Note out of MIDI playable range,
-- invertChord does nothing and returns the original list
invertChord :: [Note] -> [Note]
invertChord [] = []
invertChord (n:ns)
  | midiPlayable (n+12) == n = n:ns
  | otherwise                = ns ++ [n+12]

-- openChord takes a list of Notes and spreads them out by
-- lowering every other Note (except the last Note) by an octave
-- Notes will not lower out of MIDI playable range
openChord :: [Note] -> [Note]
openChord [] = []
openChord [n] = [n]
openChord (n:ns) = sort $
  (midiPlayable (n-12)):(head ns):(openChord (tail ns))

-- powerChord removes all but the first and last entries in a list
-- of Notes.
-- When this is the first modifier applied to a triad built from a
-- 7 note scale, it produces a power chord (hence the name)
powerChord :: [Note] -> [Note]
powerChord [] = []
powerChord [n] = [n]
powerChord ns = (head ns):(last ns):[]

-- chordUp raises all Notes in a list by an octave
-- if any notes would have been raised out of midiPlayable range,
-- the original chord is returned
chordUp :: [Note] -> [Note]
chordUp ns
  | listIsMidiPlayable nsUp = nsUp
  | otherwise               = ns
  where nsUp = map (+12) ns

-- chordDown lowers all Notes in a list by an octave
-- if any notes would have been lowered out of midiPlayable range,
-- the original chord is returned
chordDown :: [Note] -> [Note]
chordDown ns
  | listIsMidiPlayable nsDown = nsDown
  | otherwise                 = ns
  where nsDown = map (+(-12)) ns
