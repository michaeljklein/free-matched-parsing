{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE RoleAnnotations #-}
-- {-# LANGUAGE EmptyDataDecls #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE Trustworthy #-}
-- {-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Matched where

import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Control.Applicative (Alternative(..), liftA2)
import Control.Comonad (Comonad(..))
import Control.Comonad.Cofree.Class (ComonadCofree(..))
import Control.Comonad.Env (ComonadEnv(..), Env(..), EnvT(..))
import Control.Comonad.Hoist.Class (ComonadHoist(..))
import Control.Comonad.Trans.Cofree (CofreeF(..), CofreeT(..), transCofreeT)
import Control.Foldl (Fold(..), FoldM(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Free.Church (F(..), hoistF)
import Control.Monad.Free.Class (MonadFree(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Data (Data(..), Typeable(..))
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import Data.Functor.Identity (Identity(..))
import Data.Pointed (Pointed(..))
import Data.Profunctor (Choice(..), Profunctor(..))
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies(..))
import Data.String (IsString(..))
import Data.Word (Word16)
import GHC.TypeLits (type (+), Nat)
import Generics.SOP.BasicFunctors (I(..))
import Generics.SOP.NS (NS(..))
import Data.Foldable (foldl')
import Data.Constraint
import Data.Constraint.Nat
import GHC.TypeNats
import Control.Monad.Primitive (PrimMonad(..))
import Data.Vector.Generic.Mutable.Base (MVector(..))

-- | Dummy type for non-empty ranges
data Range1 a


-- Ok, so first we want to index the sum by a list of values
-- Next, we want to separate the complete and incomplete into two separate layers

-- | We wncode the following GADT using `F` and `EnvT`:
--
-- @
-- data Matched s f a where
--   NonMatched :: a -> Matched s f a
--   Matched :: s -> f (Matched s f a) -> Matched s f a
-- @
--
newtype MatchedT s f a = MatchedT { runMatchedT :: F (EnvT s f) a } deriving (Functor, Applicative, Monad, MonadFix, Foldable, Traversable)

mapEnvT :: (s -> t) -> EnvT s f a -> EnvT t f a
mapEnvT f ~(EnvT x xs) = EnvT (f x) xs

hoistEnvT :: (forall t. f t -> g t) -> EnvT s f a -> EnvT s g a
hoistEnvT f ~(EnvT x xs) = EnvT x (f xs)

withMatchedT :: (F (EnvT s f) a -> F (EnvT t g) b) -> MatchedT s f a -> MatchedT t g b
withMatchedT f = MatchedT . f . runMatchedT

mapMatchedT :: (s -> t) -> MatchedT s f a -> MatchedT t f a
mapMatchedT f = withMatchedT $ hoistF (mapEnvT f)

hoistMatchedT :: Functor f => (forall t. f t -> g t) -> MatchedT s f a -> MatchedT s g a
hoistMatchedT f = withMatchedT $ hoistF (hoistEnvT f)

wrapMatchedT :: Functor f => EnvT s f (MatchedT s f a) -> MatchedT s f a
wrapMatchedT = MatchedT . wrap . fmap runMatchedT

newtype Matched (s :: [*]) f a = Matched { runMatched :: MatchedT (NS I s) f a } deriving (Functor, Applicative, Monad, MonadFix, Foldable, Traversable)

-- The next goals are to:
--  1. Encode a partial layer for MatchedT

-- input is
-- - a finite foldable of Char-likes
-- - a finite stack of open locations
-- - a non-matched (Fold) parser
-- - implicit monadic context allowing updates of output containers
--
-- output is
-- - a finite stack of open locations
-- - an output container: f (begin, Maybe end, Either matchtype a)
-- - implicit monadic effects updating closed matches


class Traversable s => Stack s where
  pop :: s a -> Maybe a
  pop = either Just (const Nothing) . foldl' (\x y -> x >> Left y) (Right ())
  push :: s a -> a -> s a

newtype FoldRemainder w m = FoldRemainder { runFoldRemainder :: forall b. FoldM m (w Char) b -> m b }

newtype ParserT w m a = Parser { runParser :: FoldM (ExceptT (a, FoldRemainder w m) m) (w Char) a }

-- | Those sum elements that we can `reify` to `Match`
type HasMatch s = Reifies s Match

data Match = Match { matchOpen :: !Char
                   , matchClose :: !Char
                   } deriving (Eq, Ord, Show, Read)

instance IsString Match where
  fromString [matchOpen', matchClose'] = Match matchOpen' matchClose'
  fromString str =
    error $ concat ["Expected two characters, but got:", show (length str)]

-- | Location
newtype Loc = Loc { getLoc :: Word16 }

-- | Location range
newtype LocR = LocR { getLocR :: Range1 Loc }

class MonoidN s where
  memptyN :: s 0
  mappendN :: s n -> s m -> s (n + m)

class MonoidN s => StackN s e where
  pushN :: e -> s n -> s (n + 1)
  popN :: s (n + 1) -> (e, s n)

-- | `Nat`-indexed strict stack of `Int`s
data IntStack (n :: Nat) where
  EmptyIntStack :: IntStack 0
  PushIntStack :: !Int -> !(IntStack n) -> IntStack (n + 1)

instance MonoidN IntStack where
  memptyN = EmptyIntStack
  mappendN EmptyIntStack = id
  mappendN (PushIntStack x xs) = PushIntStack x . mappendN xs

instance StackN IntStack Int where
  pushN = PushIntStack
  popN (PushIntStack x xs) = (x, xs)

newtype UnmatchedT s (unmatched :: Nat) m a = UnmatchedT { runUnmatchedT :: m (a, s unmatched) } deriving (Functor)

matchWith ::
     (Monad m, StackN s e)
  => (e -> i -> m ())
  -> i
  -> UnmatchedT s (1 + unmatched) m a
  -> UnmatchedT s unmatched m a
matchWith updateMatch loc UnmatchedT {..} =
  UnmatchedT $ do
    ~(unmatched, unmatchedStack) <- runUnmatchedT
    let ~(indx, unmatchedStack') = popN unmatchedStack
    updateMatch indx loc
    return (unmatched, unmatchedStack')

newtype LineT v (lineNo :: Nat) m a = LineT { runLineT :: m (v (PrimState m) (Env LocR a)) }


-- match :: (PrimMonad m, MVector v a) => Int -> Loc -> LineT v l (UnmatchedT (u + 1) m) a -> LineT v l (UnmatchedT u m) a


-- toLinesT :: FoldM m (LineR v lineNo  FoldM m a







-- -- (isOpen, which sum)
-- parseNS :: (Elem s ns, Reifies s Match) => proxy s -> PerserT w m (Bool, NS ns)
-- parseNS = do
--   Match{..} = reify ns
--   char matchOpen >> return (True, ns)
--   char matchClose >> return (False, ns)




-- parseMatched :: (Foldable f, Monad m, MonadStack m Loc, ComonadEnv w Loc) => f (w Char) -> (Loc -> m ()) ->  -> m (LocT g (Either (NS ns) a))
-- parseMatched = _



-- -- (Functor f, Comonad w) => ComonadCofree f (CofreeT f w)Source
-- Functor f => ComonadHoist (CofreeT f)Source
-- ComonadTrans (CofreeT f)Source
-- Alternative f => MonadTrans (CofreeT f)Source
-- (Alternative f, Monad w) => Monad (CofreeT f w)Source
-- (Functor f, Functor w) => Functor (CofreeT f w)Source
-- (Alternative f, Applicative w) => Applicative (CofreeT f w)Source
-- (Foldable f, Foldable w) => Foldable (CofreeT f w)Source
-- (Traversable f, Traversable w) => Traversable (CofreeT f w)Source
-- (Alternative f, MonadZip f, MonadZip m) => MonadZip (CofreeT f m)Source
-- (Functor f, Comonad w) => Comonad (CofreeT f w)Source
-- Eq (w (CofreeF f a (CofreeT f w a))) => Eq (CofreeT f w a)Source
-- (Typeable (* -> *) f, Typeable (* -> *) w, Typeable * a, Data (w (CofreeF f a (CofreeT f w a))), Data a) => Data (CofreeT f w a)Source
-- Ord (w (CofreeF f a (CofreeT f w a))) => Ord (CofreeT f w a)Source
-- Read (w (CofreeF f a (CofreeT f w a))) => Read (CofreeT f w a)Source
-- Show (w (CofreeF f a (CofreeT f w a))) => Show (CofreeT f w a)



-- class Functor f => Corecursive1 f where
--   corecursive1 :: forall a. Dict Corecursive (f a)

-- data Matched i f a where
--   Matched :: i -> f (Matched i f a) -> Matched i f a
--   UnMatched :: i -> Fold (Matched i f a) (f (Matched i f a)) -> Matched i f a
--   NonMatched :: a -> Matched i f a

-- class IsMatched a where
--   openMatched :: Char -> Maybe a
--   closeMatched :: Char -> Maybe a

-- snocMatched :: (IsMatched i, Corecursive1 f) => Matched i f Char -> Char -> Matched i f Char
-- snocMatched (Matched matchType matches) c = maybe (NonMatched c) (flip UnMatched $ _) $ openMatched c
-- snocMatched (UnMatched matchType continue) c = maybe (stepFold continue $ NonMatched c) (\matchType' -> if matchType == matchType' then Matched matchType (extract continue) else stepFold continue $ UnMatched ..)



-- newtype Matched i f a = CofreeF f i

-- Cofree f (Either i a)

-- [Matched matchType begin (maybe end) | NonMatched begin end]
-- [(matchType, beginLocation)]
-- [(matchType, endLocation)]


-- -- plusCommutes2 :: forall (m1 :: Nat) (n1 :: Nat) (m2 :: Nat) (n2 :: Nat).  ((m1 + n1) ~ (m2 + n2)) :- ((n1 + m1) ~ (n2 + m2))
-- plusCommutes2 = eqCommutes . mapEq plusCommutes . eqCommutes . mapEq plusCommutes

-- eqCommutes :: forall (a :: k) (b :: k). (a ~ b) :- (b ~ a)
-- eqCommutes = unmapDict $ \dict -> withDict dict Dict

-- transEq :: forall (a :: k) (b :: k) (c :: k) (d :: k). Dict (a ~ c) -> Dict (b ~ d) -> Dict (a ~ b) -> Dict (c ~ d)
-- transEq lEq rEq dict = withDict lEq (withDict rEq (withDict dict Dict))

-- mapEq :: Dict (b ~ c) -> (a ~ b) :- (a ~ c)
-- mapEq dict = withDict dict (unmapDict $ \dict' -> withDict dict' Dict)

-- -- (n0 + n) ~ (n0 + m) :- n ~ m

-- -- (n + 1) ~ (1 + n), (m + 1) ~ (1 + m),
-- -- plusCommutes :: forall n m. Dict ((m + n) ~ (n + m))

-- -- plusIsCancellative :: forall n m o. ((n + m) ~ (n + o)) :- (m ~ o)



