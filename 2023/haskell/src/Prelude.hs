module Prelude (module P) where

import Control.Applicative        as P (liftA, liftA2)
import Control.Arrow              as P (first, left, second, (&&&), (***),
                                        (<<<), (>>>))
import Control.Monad.Except       as P
import Control.Monad.Identity     as P
import Control.Monad.Reader       as P
import Control.Monad.State.Strict as P
import Control.Monad.Trans.Maybe  as P (MaybeT (..))
import Data.Bifunctor             as P (bimap)
import Data.Bool                  as P (bool)
import Data.Either                as P (lefts, partitionEithers, rights)
import Data.Foldable              as P (asum, fold, foldMap', foldlM, foldrM,
                                        for_, toList, traverse_)
import Data.Function              as P (on, (&))
import Data.Functor               as P (($>), (<&>))
import Data.Functor.Const         as P (Const (..))
import Data.Hashable              as P (Hashable)
import Data.HashMap.Strict        as P (HashMap, mapKeys)
import Data.HashSet               as P (HashSet)
import Data.List                  as P (find, findIndex, foldl', group,
                                        intercalate, intersect, intersperse,
                                        lookup, sort, sortBy, sortOn, union,
                                        unionBy, (\\))
import Data.List.NonEmpty         as P (NonEmpty (..), nonEmpty)
import Data.Maybe                 as P (catMaybes, fromMaybe, isJust, isNothing,
                                        listToMaybe, mapMaybe, maybeToList)
import Data.Ord                   as P (comparing)
import Data.Semigroup             as P (Semigroup (..))
import Data.Sequence              as P (Seq)
import Data.String                as P (IsString)
import Data.Text                  as P (Text)
import Data.Traversable           as P (for)
import Data.Void                  as P (Void, absurd)
import GHC.Generics               as P (Generic)
import "base" Prelude             as P hiding (lookup)
