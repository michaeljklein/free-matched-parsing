{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Matched.Cofree where

import Data.Profunctor (Choice(..), Profunctor(..))
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree
import Control.FoldF

-- | Input is finite foldable and what to do with the gaps between matched.
-- Output is matched
data MatchType = None
               | Parens
               | Bracket
               | Brace
               deriving (Eq, Ord, Show, Read, Enum)

-- | Matches on inputs of type @a@ with shape @f@ and results of type @b@
--
-- @
--  data Matched f a where
--    Matched :: !MatchType -> f (Matched f a) -> Matched f a
--    UnMatched :: !MatchType -> Fold (Matched f a) (f (Matched f a)) -> Matched f a
-- @
--
newtype Matched a f b = Matched
  { runMatched :: CofreeT (FoldF f a) (Env MatchType) b
  } deriving (Functor)

preMatched :: Functor f => (a -> b) -> Matched b f c -> Matched a f c
preMatched f = Matched . transCofreeT (lmap f) . runMatched

-- -- instance Applicative (Matched f) where

-- -- instance Monad (Matched f) where

-- -- instance MonadZip (Matched f) where

-- -- instance MonadTrans Matched where

-- instance Foldable (Matched f) where
--   foldr f x (Matched y) = _ f x y

-- instance Traversable f => Traversable (Matched f) where
--   traverse f (Matched (CofreeT (EnvT x (Identity (y :< ys))))) = Matched . CofreeT . EnvT x . Identity <$> ((:<) <$> f y <*> _ f ys)

instance Functor f => Comonad (Matched a f) where
  extract = extract . runMatched
  duplicate = Matched . extend Matched . runMatched

instance Functor f => ComonadEnv MatchType (Matched a f) where
  ask = ask . runMatched

instance Functor f => ComonadCofree (FoldF f a) (Matched a f) where
  unwrap :: Functor f => Matched a f b -> FoldF f a (Matched a f b)
  unwrap = fmap Matched . unwrap . runMatched




