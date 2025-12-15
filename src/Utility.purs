module Utility where

import Prelude

import Data.Profunctor.Strong (class Strong)
import Type.Prelude (class IsSymbol, Proxy(..))
import Prim.Row (class Cons)
import Data.Lens.Record (prop)
import Partial.Unsafe (unsafeCrashWith)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

prop' :: forall @l r1 r2 r a b. IsSymbol l ⇒ Cons l a r r1 ⇒ Cons l b r r2 ⇒ (forall p. Strong p ⇒ p a b → p (Record r1) (Record r2))
prop' = prop (Proxy @l)

