-- @inline export build arity=2
-- @inline export api arity=1
-- @inline export run arity=1

module Transit.HandlerLookup
  ( HandlerLookup
  , initBuilder
  , addHandler
  , build
  , HandlerLookupBuilder
  , run
  ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype HandlerLookupBuilder m (rowState :: Row Type) (rowMsg :: Row Type) =
  HandlerLookupBuilder
    ( List
        { state :: String
        , msg :: String
        , handler :: Handler m rowState
        }
    )

type Handler m rowState = Foreign -> Foreign -> m (Variant rowState)

newtype HandlerLookup m (rowState :: Row Type) (rowMsg :: Row Type) = HandlerLookup (Object (Object (Handler m rowState)))

initBuilder :: forall @rowState @rowMsg m. HandlerLookupBuilder m rowState rowMsg
initBuilder = HandlerLookupBuilder $ List.fromFoldable []

addHandler
  :: forall @symStaIn @symMsgIn m staIn msgIn rowStateOut rowMsg rowState trashState trashMsg trashUnion
   . IsSymbol symStaIn
  => IsSymbol symMsgIn
  --=> Row.Cons symStaIn staIn trashState rowState
  --=> Row.Cons symMsgIn msgIn trashMsg rowMsg
  --=> Row.Union rowStateOut trashUnion rowState
  => (staIn -> msgIn -> m (Variant rowStateOut))
  -> HandlerLookupBuilder m rowState rowMsg
  -> HandlerLookupBuilder m rowState rowMsg
addHandler handlerHigh (HandlerLookupBuilder builders) =
  HandlerLookupBuilder $ List.Cons head builders
  where
  head =
    { state: reflectSymbol (Proxy @symStaIn)
    , msg: reflectSymbol (Proxy @symMsgIn)
    , handler: handlerLow
    }

  handlerLow :: Foreign -> Foreign -> m (Variant rowState)
  handlerLow = unsafeCoerce handlerHigh

build :: forall m rowState rowMsg. Applicative m => HandlerLookupBuilder m rowState rowMsg -> HandlerLookup m rowState rowMsg
build (HandlerLookupBuilder builders) = HandlerLookup
  $ Object.fromFoldableWith Object.union
  $
    map (\{ state, msg, handler } -> (state /\ Object.singleton msg handler)) builders

type MaybeSpec a =
  { onNothing :: Maybe a
  , onJust :: a -> Maybe a
  }

type T m a =
  { fail :: m (Maybe a)
  , succeed :: m a -> m (Maybe a)
  }

api :: forall m a. Applicative m => T m a
api = { fail: pure Nothing, succeed: map Just }

run
  :: forall m rowState rowMsg
   . Applicative m
  => HandlerLookup m rowState rowMsg
  -> Variant rowState
  -> Variant rowMsg
  -> m (Maybe (Variant rowState))
run = runImpl api

foreign import runImpl
  :: forall m rowState rowMsg
   . T m (Variant rowState)
  -> HandlerLookup m rowState rowMsg
  -> Variant rowState
  -> Variant rowMsg
  -> m (Maybe (Variant rowState))

