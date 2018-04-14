module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Uncurried (EffFn3, runEffFn3, EffFn2, runEffFn2, EffFn1, runEffFn1)
import Data.Foreign (Foreign, toForeign)
import Node.Process (PROCESS, lookupEnv)
import Simple.JSON (write, class WriteForeign)
import Data.Maybe (maybe)

-- debug
import Debug.Trace (traceAnyA)
-- import Unsafe.Coerce (unsafeCoerce)


foreign import data Context ∷ Type
foreign import data Raven ∷ Type
foreign import data RAVEN ∷ Effect

newtype Dsn = Dsn String

foreign import ravenImpl ∷ ∀ eff. EffFn1 (raven ∷ RAVEN | eff) String Raven

foreign import captureMessageImpl ∷ ∀ eff. EffFn2 (raven ∷ RAVEN | eff) Raven String Unit

foreign import inContextImpl ∷ ∀ a eff. EffFn3 (raven ∷ RAVEN | eff) Raven Foreign (Eff (raven ∷ RAVEN | eff) a) a

foreign import mergeContextImpl ∷ ∀ eff. EffFn2 (raven ∷ RAVEN | eff) Raven Foreign Unit

foreign import setContextImpl ∷ ∀ eff. EffFn2 (raven ∷ RAVEN | eff) Raven Foreign Unit

foreign import getContextImpl ∷ ∀ eff. EffFn1 (raven ∷ RAVEN | eff) Raven Context

-- czy to jest bezpieczne
-- foreign import extendContextImpl ∷ Context → Foreign → Context

-- debug
foreign import throw ∷ ∀ eff. Eff eff Int

instance writeForeighnContextInstance ∷ WriteForeign Context where
  writeImpl = toForeign

raven ∷ ∀ eff. Dsn → Eff (raven ∷ RAVEN | eff) Raven
raven (Dsn s) = runEffFn1 ravenImpl s

captureMessage ∷ ∀ eff. Raven → String → Eff (raven ∷ RAVEN | eff) Unit
captureMessage = runEffFn2 captureMessageImpl

-- context
inContext ∷ ∀ a ctx eff.  WriteForeign ctx ⇒ Raven → ctx → Eff (raven ∷ RAVEN | eff) a → Eff (raven ∷ RAVEN | eff) a
inContext rvn ctx = runEffFn3 inContextImpl rvn (write ctx)

setContext ∷ ∀ ctx eff. WriteForeign ctx ⇒ Raven → ctx → Eff (raven ∷ RAVEN | eff) Unit
setContext rvn ctx = runEffFn2 setContextImpl rvn (write ctx)

mergeContext ∷ ∀ ctx eff. WriteForeign ctx ⇒ Raven → ctx → Eff (raven ∷ RAVEN | eff) Unit
mergeContext rvn ctx = runEffFn2 mergeContextImpl rvn (write ctx)

-- extendContext ∷ ∀ ctx. WriteForeign ctx ⇒ Context → ctx → Context
-- extendContext old new = extendContextImpl old (write new)

getContext ∷ ∀ eff. Raven → Eff (raven ∷ RAVEN | eff) Context
getContext = runEffFn1 getContextImpl


---------- main for tests
main ∷ forall e. Eff (console ∷ CONSOLE, process ∷ PROCESS, raven ∷ RAVEN | e) Unit
main = do
  dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"
  r ← raven dsn
  setContext r {user : {id : 1}}
  captureMessage r ("TEST MESSAGE")
  setContext r {tags : {testTagKey : "testTagValue"}}
  ctx ← getContext r
  traceAnyA ctx

  captureMessage r ("TEST MESSAGE 2")
  mergeContext r {user : {id : "testUserId"}}


  captureMessage r ("TEST MESSAGE 3")

  mergeContext r {}
  captureMessage r ("TEST MESSAGE 4")

  setContext r {}
  captureMessage r ("TEST MESSAGE T")

  -- _ ← throw   ------------------- odkomentowanie tej linijki powoduje, że żaden message ani error nie jest logowany

  x ← inContext r ctx (do
                        ctx ← getContext r
                        mergeContext r {user : {email : "test@test.com"}}
                        log "ctx2:"
                        traceAnyA ctx -- shows: {}
                        throw)

  logShow x
  -- traceAnyA x
  -- captureMessage r "NO GLOB"
  -- traceAnyA r



-- named args - phils trick
-- f u =
--   let
--     user = u { id: Nothing, email: Nothing, age: Nothing }
--   in
--     user
--
--
-- u = f _{ email = Just "new", age = Just 8 }
