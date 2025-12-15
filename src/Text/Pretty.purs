module Text.Pretty where

import Prelude

import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.String (Pattern(..), joinWith, split)
import Data.UUID (UUID)

class Pretty a where
  pretty :: a -> String

instance Pretty Unit where
  pretty = show

instance Pretty Boolean where
  pretty = show

instance Pretty Int where
  pretty = show

instance Pretty String where
  pretty = show

instance Pretty UUID where
  pretty = show

instance Pretty Void where
  pretty = absurd

indent :: String -> String
indent = split (Pattern "\n") >>> map ("    " <> _) >>> joinWith "\n"

unLines2 :: forall f. Foldable f => f String -> String
unLines2 = fromFoldable >>> joinWith "\n\n"

commas :: forall f. Foldable f => f String -> String
commas = fromFoldable >>> joinWith ", "