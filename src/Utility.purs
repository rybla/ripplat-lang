module Utility where

import Prelude

import Control.Monad.RWS (RWSResult, RWST, runRWST)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, foldl)
import Data.Lens.Record (prop)
import Data.List.Types (List(..))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Type.Prelude (class IsSymbol, Proxy(..))

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

todoK :: forall a b. String -> a -> b
todoK msg _ = todo msg

prop' :: forall @l r1 r2 r a b. IsSymbol l ⇒ Cons l a r r1 ⇒ Cons l b r r2 ⇒ (forall p. Strong p ⇒ p a b → p (Record r1) (Record r2))
prop' = prop (Proxy @l)

partitionEither
  :: forall f a b c
   . Foldable f
  => (a -> b \/ c)
  -> f a
  -> List b /\ List c
partitionEither f = foldl
  (\(bs /\ cs) -> either (\b -> (Cons b bs /\ cs)) (\c -> (bs /\ Cons c cs)) <<< f)
  (none /\ none)

runRWST' :: forall r w s m a. RWST r w s m a -> (r /\ s) -> m (RWSResult s a w)
runRWST' m (r /\ s) = runRWST m r s

unimplemented :: forall a. String -> a
unimplemented msg = unsafeCrashWith $ "Unimplemented: " <> msg

