# free-matched-parsing

`Free` and `Cofree`-based matched expression AST parsing and representation.

The main idea is to encode the "matched part" of an AST as a
recursive data type, where we can plug in parsers and representation
`Functor`s later, resulting in a complete AST.


## Control.FoldF

Folding with a `f`-wrapped result and explicit encoding of constant folds:

```haskell
newtype FoldF f a b = FoldF
  { runFoldF :: Either (f b) (Fold a (f b))
  }

pure :: Applicative f => b -> FoldF f a b
pure = FoldF . Left . pure

liftFoldF :: f b -> FoldF f a b

Functor f => Functor (FoldF f a)
Functor f => Profunctor (FoldF f)
(Functor f, Pointed f) => Choice (FoldF f)

Comonad f => Comonad (FoldF f a)

Applicative f => Applicative (FoldF f a)
Alternative f => Alternative (FoldF f a)
```


## Data.Buffered

Prototypes for `Cofree` monadic streams of low-level buffered input.


## Data.Matched

Various implementations of matched ASTs, including ones:
 - Based on the free monad
 - That use type-level naturals to represent thair stack depth
 - That use reflection to index match types

This module also includes prototype folds and parsers to produce the ASTs
from various input representations.


## Data.Matched.Cofree

An implementation of a recursive fold resulting in a `Matched` AST:

```haskell
newtype Matched a f b = Matched
  { runMatched :: CofreeT (FoldF f a) (Env MatchType) b
  } deriving (Functor)
```


The matches are indexed by `MatchType` and we have:

```haskell
Functor f => Comonad (Matched a f)
Functor f => ComonadEnv MatchType (Matched a f)
Functor f => ComonadCofree (FoldF f a) (Matched a f)
```


# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/free-matched-parsing/)

