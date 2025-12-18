module Text.Pretty where

import Prelude

import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Maybe (maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.UUID (UUID)
import Options.Applicative.Internal.Utils (unLines)

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

instance Pretty a => Pretty (List a) where
  pretty l = "[" <> (commas $ map pretty l) <> "]"

instance Pretty a => Pretty (Array a) where
  pretty l = "[" <> (commas $ map pretty l) <> "]"

indentationBullet :: String
indentationBullet = "  - "

indentation :: String
indentation = "    "

indent :: String -> String
indent = split (Pattern "\n") >>> map (indentation <> _) >>> joinWith "\n"

unLines2 :: forall f. Foldable f => f String -> String
unLines2 = fromFoldable >>> joinWith "\n\n"

commas :: forall f. Foldable f => f String -> String
commas = fromFoldable >>> joinWith ", "

spaces :: forall f. Foldable f => f String -> String
spaces = fromFoldable >>> joinWith " "

quoteCode :: String -> String
quoteCode s = "`" <> s <> "`"

bullets :: forall f. Functor f => Foldable f => f String -> String
bullets = map ("  - " <> _) >>> unLines

indentBullet :: String -> String
indentBullet =
  split (Pattern "\n")
    >>> Array.uncons
    >>> maybe mempty (\{ head, tail } -> Array.cons (indentationBullet <> head) (tail # map (indentation <> _)))
    >>> joinWith "\n"

paren :: String -> String
paren s = "(" <> s <> ")"