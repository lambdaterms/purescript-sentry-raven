module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2, EffFn1, runEffFn1)
import Node.Process (PROCESS, lookupEnv)
import Data.Maybe (maybe)
import Debug.Trace (traceAnyA)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Raven ∷ Type
foreign import data RAVEN ∷ Effect

newtype Dsn = Dsn String

foreign import ravenImpl ∷ ∀ eff. EffFn1 (raven ∷ RAVEN | eff) String Raven

foreign import captureMessageImpl ∷ ∀ eff. EffFn2 (raven ∷ RAVEN | eff) Raven String Unit

foreign import inContextImpl ∷ ∀ a eff. EffFn2 (raven ∷ RAVEN | eff) Raven (Eff (raven ∷ RAVEN | eff) a) a

raven ∷ ∀ eff. Dsn → Eff (raven ∷ RAVEN | eff) Raven
raven (Dsn s) = runEffFn1 ravenImpl s

captureMessage ∷ ∀ eff. Raven → String → Eff (raven ∷ RAVEN | eff) Unit
captureMessage r msg = runEffFn2 captureMessageImpl r msg

inContext ∷ ∀ a eff. Raven → Eff (raven ∷ RAVEN | eff) a → Eff (raven ∷ RAVEN | eff) a
inContext r eff = runEffFn2 inContextImpl r eff
main ∷ forall e. Eff (console ∷ CONSOLE, process ∷ PROCESS, raven ∷ RAVEN | e) Unit
main = do
  dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"

foreign import throw ∷ ∀ eff. Eff eff Int



-- named args - phils trick
-- f u =
--   let
--     user = u { id: Nothing, email: Nothing, age: Nothing }
--   in
--     user
--
--
-- u = f _{ email = Just "new", age = Just 8 }


  captureMessage r ("TEST MESSAGE")
  -- x ← y r
  x ← inContext r throw
  logShow x
  -- traceAnyA x
  -- captureMessage r "NO GLOB"
  -- traceAnyA r
