module Handy.Prelude
  ( (∘), (×)
  , type (×)
  , As, As1
  , asList, asArray
  , case2_
  , show
  , spyWith
  , module Prelude
  , module Control.Alt
  , module Control.Apply
  , module Control.Bind
  , module Control.Monad
  , module Control.Monad.Error.Class
  , module Control.Monad.Except
  , module Control.Monad.Gen.Class
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.Reader
  , module Control.Monad.Rec.Class
  , module Control.Monad.Trans.Class
  , module Control.MonadPlus
  , module Control.Parallel
  , module Control.Plus
  , module Data.Array.NonEmpty
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Const
  , module Data.Either
  , module Data.Eq
  , module Data.Foldable
  , module Data.FoldableWithIndex
  , module Data.Foldable1
  , module Data.Functor
  , module Data.Functor.Coproduct
  , module Data.FunctorWithIndex
  , module Data.Functor1
  , module Data.Generic.Rep
  , module Data.HeytingAlgebra
  , module Data.Identity
  , module Data.Identity1
  , module Data.Lens
  , module Data.Lens.Iso.Newtype
  , module Data.List
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Map
  , module Data.Monoid
  , module Data.Newtype
  , module Data.Ord
  , module Data.Profunctor
  , module Data.Semigroup.Foldable
  , module Data.Set
  , module Data.Set.NonEmpty
  , module Data.String.NonEmpty
  , module Data.Symbol
  , module Data.These
  , module Data.Traversable
  , module Data.TraversableWithIndex
  , module Data.Traversable1
  , module Data.Tuple
  , module Data.Variant
  , module Data.Void
  , module Debug
  , module Effect
  , module Effect.Class
  , module Effect.Aff
  , module Effect.Aff.Class
  , module Foreign.Object
  , module Partial.Unsafe
  , module Type.Row
  )
  where

import Prelude hiding (show)

import Control.Alt (class Alt, (<|>))
import Control.Apply ((*>), (<*))
import Control.Bind (join, (>=>), (<=<))
import Control.Monad (when, unless)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT, except)
import Control.Monad.Gen.Class (class MonadGen)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Parallel (class Parallel, parTraverse, parTraverse_)
import Control.Plus (class Plus, empty)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifoldable (class Bifoldable, bitraverse_, bifor_)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Bitraversable (class Bitraversable, bitraverse, bisequence, bifor)
import Data.Const (Const(..))
import Data.Either (Either(..), either, isLeft, isRight, fromRight, note, hush)
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, traverse_, for_, foldMap, foldl, foldr, fold)
import Data.FoldableWithIndex (foldrWithIndex, foldlWithIndex, foldMapWithIndex)
import Data.Foldable1 (class Foldable1, foldl1, foldr1, foldMap1)
import Data.Functor (($>), (<$))
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Functor1 (class Functor1, map1)
import Data.Generic.Rep (class Generic)
import Data.HeytingAlgebra (tt, ff)
import Data.Identity (Identity(..))
import Data.Identity1 (Identity1(..), mkIdentity1)
import Data.Lens (Lens, Lens', Prism, Prism', Traversal, Traversal', Optic, (.~), (?~), (^.), (^?), (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, ala, alaF, un)
import Data.Ord (class Ord1, compare1)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Semigroup.Foldable (fold1)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Show as Show
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (class IsSymbol)
import Data.These (These(..))
import Data.Traversable (class Traversable, traverse, sequence, for)
import Data.TraversableWithIndex (traverseWithIndex, forWithIndex)
import Data.Traversable1 (class Traversable1, traverse1)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Variant (Variant, case_, class Contractable, contract, expand)
import Data.Void (Void, absurd)
import Debug (class DebugWarning, spy, trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, ParAff, joinFiber, forkAff)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (liftEffect, class MonadEffect)
import Foreign.Object (Object)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError as TE
import Type.Row (type (+))

infixr 9 compose as ∘
infixr 1 Tuple as ×
infixr 4 type Tuple as ×

type As a = a → a

type As1 f = f ~> f

asList = identity ∷ As1 List
asArray = identity ∷ As1 Array

case2_ ∷ ∀ a. Variant () → Variant () → a
case2_ _ = case_

show ∷ ∀ a. TE.Warn (TE.Text "Show usage") ⇒ Show a ⇒ a → String
show = Show.show

spyWith ∷ ∀ a b. DebugWarning ⇒ String → (a → b) → a → a
spyWith msg f a = const a (spy msg (f a))
