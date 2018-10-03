{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Control.FoldF where

import Data.Profunctor (Choice(..), Profunctor(..))
import Control.Comonad
import Control.Foldl (Fold(..), FoldM(..), fold)
import Control.Applicative (Alternative(..), liftA2)
import Data.Pointed (Pointed(..))
import Data.Bifunctor (Bifunctor(..))


newtype FoldF f a b = FoldF
  { runFoldF :: Either (f b) (Fold a (f b))
  }

instance Functor f => Functor (FoldF f a) where
  fmap f (FoldF x) = FoldF $ bimap (fmap f) (fmap f <$>) x

instance Functor f => Profunctor (FoldF f) where
  dimap :: (a -> b) -> (c -> d) -> FoldF f b c -> FoldF f a d
  dimap f g (FoldF x) = FoldF $ bimap (fmap g) (dimap f (fmap g)) x

instance (Functor f, Pointed f) => Choice (FoldF f) where
  left' :: FoldF f a b -> FoldF f (Either a c) (Either b c)
  left' = FoldF . bimap (fmap Left) (fmap (either (fmap Left) (point . Right)) . left') . runFoldF

  right' :: FoldF f a b -> FoldF f (Either c a) (Either c b)
  right' = FoldF . bimap (fmap Right) (fmap (either (point . Left) (fmap Right)) . right') . runFoldF

instance Comonad f => Comonad (FoldF f a) where
  extract :: FoldF f a b -> b
  extract = either extract (extract . extract) . runFoldF

  duplicate  (FoldF (Left  x)) = FoldF . Left  . extend (FoldF . Left) $ x
  duplicate ~(FoldF (Right x)) = FoldF . Right . fmap (extend (FoldF . Left)) $ x

instance Applicative f => Applicative (FoldF f a) where
  pure = FoldF . Left . pure

  (<*>) = apFoldF (<*>)

instance Alternative f => Alternative (FoldF f a) where
  empty = FoldF . Left $ empty

  (<|>) = apFoldF (<|>)

liftFoldF :: f b -> FoldF f a b
liftFoldF = FoldF . Left

apFoldF :: (f a -> g b -> h c) -> FoldF f t a -> FoldF g t b -> FoldF h t c
apFoldF f  (FoldF (Left  x))  (FoldF (Left  y)) = FoldF . Left $ f x y
apFoldF f  (FoldF (Left  x))  (FoldF (Right y)) = FoldF . Right $ f x <$> y
apFoldF f  (FoldF (Right x))  (FoldF (Left  y)) = FoldF . Right $ (`f` y) <$> x
apFoldF f ~(FoldF (Right x)) ~(FoldF (Right y)) = FoldF . Right $ liftA2 f x y

hoistFoldF :: Functor f => (forall t. f t -> g t) -> FoldF f a b -> FoldF g a b
hoistFoldF f (FoldF x) = FoldF $ bimap f (fmap f) x

mapFoldF :: (f b -> g d) -> (Fold a (f b) -> Fold c (g d)) -> FoldF f a b -> FoldF g c d
mapFoldF lMap rMap (FoldF x) = FoldF $ bimap lMap rMap x

-- | Apply a `FoldF` to a `Foldable` input
foldF :: Foldable t => FoldF f a b -> t a -> f b
foldF (FoldF fs) xs = case fs of
                       Left ys -> ys
                       ~(Right f) -> fold f xs

