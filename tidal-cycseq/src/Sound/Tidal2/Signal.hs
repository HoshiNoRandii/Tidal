{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

-- (c) Alex McLean 2022 and contributors
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal2.Signal where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Control.Applicative (liftA2)

import Sound.Tidal2.Span
import Sound.Tidal2.Value
import Sound.Tidal2.Event
import Sound.Tidal2.Pattern

import Prelude hiding ((<*), (*>))
import qualified Data.Map.Strict as Map

-- ************************************************************ --
-- State

-- | A timespan and some named control values, used to query a signal
-- with
data State = State {sSpan :: Span,
                    sControls :: ValueMap
                   }

-- ************************************************************ --
-- Signal

-- | A signal - a function from a timespan to a list of events active
-- during that timespan
-- This was known as a 'Pattern' in the previous version of Tidal. A
-- signal is a function from a timespan (possibly with some other
-- state) to events taking place in that timespan.

data Signal a = Signal {query :: State -> [Event a]}
  deriving (Functor)

instance Show a => Show (Signal a) where
  show pat = show $ querySpan pat (Span 0 1)

-- | A control signal
type ControlSignal = Signal ValueMap

-- ************************************************************ --

instance Applicative Signal where
  pure = steady
  (<*>) = app

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timespans
app :: Signal (a -> b) -> Signal a -> Signal b
app patf patv = Signal f
    where f s = concatMap (\ef -> catMaybes $ map (combine ef) $ query patv s) $ query patf s
          combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                             return $ Event {metadata = metadata ef <> metadata ev,
                                             whole = liftA2 sect (whole ef) (whole ev),
                                             active = new_active,
                                             value = value ef $ value ev
                                            }

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(<*) :: Signal (a -> b) -> Signal a -> Signal b
(<*) patf patv = Signal f
  where f s = concatMap (\ef -> catMaybes $ map (combine ef) $ query patv (s {sSpan = wholeOrActive ef})
                        ) $ query patf s
        combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ef,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }
        
-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(*>) :: Signal (a -> b) -> Signal a -> Signal b
(*>) patf patv = Signal f
  where f s = concatMap (\ev -> catMaybes $ map (combine ev) $ query patf (s {sSpan = wholeOrActive ev})
                        ) $ query patv s
        combine ev ef = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ev,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }

-- ************************************************************ --

instance Monad Signal where
  (>>=) = bind

bind :: Signal a -> (a -> Signal b) -> Signal b
bind = bindWhole (liftA2 sect)

innerBind :: Signal a -> (a -> Signal b) -> Signal b
innerBind = bindWhole const

innerJoin :: Signal (Signal a) -> Signal a
innerJoin s = innerBind s id

outerBind :: Signal a -> (a -> Signal b) -> Signal b
outerBind = bindWhole (flip const)

outerJoin :: Signal (Signal a) -> Signal a
outerJoin s = outerBind s id

bindWhole :: (Maybe Span -> Maybe Span -> Maybe Span) -> Signal a -> (a -> Signal b) -> Signal b
bindWhole chooseWhole pv f = Signal $ \state -> concatMap (match state) $ query pv state
  where match state event = map (withWhole event) $ query (f $ value event) (state {sSpan = active event})
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

-- ************************************************************ --
-- Pattern instance

instance Pattern Signal where
  slowcat = sigSlowcat
  _fast   = _sigFast
  _early  = _sigEarly
  silence = sigSilence
  atom    = sigAtom
  stack   = sigStack
  _patternify f x pat = innerJoin $ (`f` pat) <$> x

-- ************************************************************ --
-- General hacks and utilities

instance Show (a -> b) where
  show _ = "<function>"

-- ************************************************************ --
-- Time utilities

querySpan :: Signal a -> Span -> [Event a]
querySpan sig span = query sig (State span Map.empty)

withEventSpan :: (Span -> Span) -> Signal a -> Signal a
withEventSpan spanf sig = Signal f
  where f s = map (\e -> e {active = spanf $ active e,
                            whole = spanf <$> whole e
                           }) $ query sig s

withEventTime :: (Time -> Time) -> Signal a -> Signal a
withEventTime timef sig = Signal f
  where f s = map (\e -> e {active = withSpanTime timef $ active e,
                            whole = withSpanTime timef <$> whole e
                           }) $ query sig s

withSpanTime :: (Time -> Time) -> Span -> Span
withSpanTime timef (Span b e) = Span (timef b) (timef e)

withQuery :: (State -> State) -> Signal a -> Signal a
withQuery statef sig = Signal $ \state -> query sig $ statef state

withQuerySpan :: (Span -> Span) -> Signal a -> Signal a
withQuerySpan spanf = withQuery (\state -> state {sSpan = spanf $ sSpan state})

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQuerySpan (withSpanTime timef)

-- ************************************************************ --
-- Fundamental signals

sigSilence :: Signal a
sigSilence = Signal (\_ -> [])

-- | Repeat discrete value once per cycle
sigAtom :: a -> Signal a
sigAtom v = Signal $ \state -> map
                               (\span -> Event {metadata = mempty,
                                                whole = Just $ wholeCycle $ start span,
                                                active = span,
                                                value = v
                                               }
                               )
                               (splitSpans $ sSpan state)
  where wholeCycle :: Time -> Span
        wholeCycle t = Span (sam t) (nextSam t)

-- | A continuous value
steady :: a -> Signal a
steady v = waveform (const v)

-- ************************************************************ --
-- Waveforms

-- | A continuous pattern as a function from time to values. Takes the
-- midpoint of the given query as the time value.
waveform :: (Time -> a) -> Signal a
waveform timeF = Signal $ \(State (Span b e) _) -> 
  [Event {metadata = mempty,
          whole = Nothing,
          active = (Span b e),
          value = timeF $ b+((e - b)/2)
         }
  ]

-- | Sawtooth waveform
saw :: (Fractional a, Real a) => Signal a
saw = waveform $ \t -> mod' (fromRational t) 1

saw2 :: (Fractional a, Real a) => Signal a
saw2 = toBipolar saw

-- | Inverse (descending) sawtooth waveform
isaw :: (Fractional a, Real a) => Signal a
isaw = (1-) <$> saw

isaw2 :: (Fractional a, Real a) => Signal a
isaw2 = toBipolar isaw

-- | Triangular wave
tri :: (Fractional a, Real a) => Signal a
tri = fastAppend saw isaw

tri2 :: (Fractional a, Real a) => Signal a
tri2 = toBipolar tri

-- | Sine waveform
sine :: Fractional a => Signal a
sine = fromBipolar sine2

sine2 :: Fractional a => Signal a
sine2 = waveform $ \t -> realToFrac $ sin ((pi :: Double) * 2 * fromRational t)

-- | Cosine waveform
cosine :: Fractional a => Signal a
cosine = _late 0.25 sine

cosine2 :: Fractional a => Signal a
cosine2 = _late 0.25 sine2

-- | Square wave
square :: Fractional a => Signal a
square = fastAppend (steady 1) (steady 0)

square2 :: Fractional a => Signal a
square2 = fastAppend (steady (-1)) (steady 1)

-- ************************************************************ --
-- Signal manipulations

splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ \state -> (concatMap (\span -> query pat (state {sSpan = span}))
                                        $ splitSpans $ sSpan state)

-- | Concatenate a list of patterns, interleaving cycles.
sigSlowcat :: [Signal a] -> Signal a
sigSlowcat pats = splitQueries $ Signal queryCycle
  where queryCycle state = query (_late (offset $ sSpan state) (pat $ sSpan state)) state
        pat span = pats !! (mod (floor $ start $ span) n)
        offset span = (sam $ start span) - (sam $ start span / (toRational n))
        n = length pats

_sigFast :: Time -> Signal a -> Signal a
_sigFast t pat = withEventTime (/t) $ withQueryTime (*t) $ pat

_sigEarly :: Time -> Signal a -> Signal a
_sigEarly t pat = withEventTime (subtract t) $ withQueryTime (+ t) $ pat

sigStack :: [Signal a] -> Signal a
sigStack pats = Signal $ \s -> concatMap (\pat -> query pat s) pats

squash :: Time -> Signal a -> Signal a
squash into pat = splitQueries $ withEventSpan ef $ withQuerySpan qf pat
  where qf (Span s e) = Span (sam s + (min 1 $ (s - sam s) / into)) (sam s + (min 1 $ (e - sam s) / into))
        ef (Span s e) = Span (sam s + (s - sam s) * into) (sam s + (e - sam s) * into)

squashTo :: Time -> Time -> Signal a -> Signal a
squashTo b e = _late b . squash (e-b)

-- ************************************************************ --
-- Higher order transformations

--every :: Int -> (Signal a -> Signal a) -> Signal a -> Signal a
--every n f pat = splitQueries $ Signal 

-- ************************************************************ --

(#) :: ControlSignal -> ControlSignal -> ControlSignal
(#) a b = Map.union <$> a <*> b

(|+|) :: Num a => Signal a -> Signal a -> Signal a
(|+|) a b = (+) <$> a <*> b

(|+) :: Num a => Signal a -> Signal a -> Signal a
(|+) a b = ((+) <$> a) <* b

(+|) :: Num a => Signal a -> Signal a -> Signal a
(+|) a b = ((+) <$> a) *> b

-- ************************************************************ --

sound :: Signal String -> ControlSignal
sound pat = (Map.singleton "sound" . S) <$> pat

note :: Signal Double -> ControlSignal
note pat = (Map.singleton "note" . F) <$> pat

-- ************************************************************ --
