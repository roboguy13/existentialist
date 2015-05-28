{-# LANGUAGE RankNTypes, ExistentialQuantification
           , BangPatterns, PatternSynonyms, TypeOperators
           , TypeFamilies, PolyKinds, MultiParamTypeClasses
           , FlexibleInstances, FlexibleContexts, GADTs
           , PartialTypeSignatures, NamedWildCards
  #-}
module Data.Existentialist
  (Existential (..)
  ,AnyList (..)
  ,toExistentials
  ,fromExistentials
  ,anyMap
  ,anyFoldr
  ,anyFoldl'
  ,anyMapM
  ,anyMapM_
  )
  where

import Control.Monad

import Data.Foldable

data Existential f = forall a. Existential (f a)


data AnyList f = forall a. (f a) :| (AnyList f)
               | Nil
infixr :|

pattern x :> xs = Existential x : xs

toExistentials :: AnyList f -> [Existential f]
toExistentials (x :| xs) = x :> toExistentials xs
toExistentials Nil       = []

fromExistentials :: Foldable g => g (Existential f) -> AnyList f
fromExistentials = go . toList
 where
    go :: [Existential f] -> AnyList f
    go (x :> xs) = x :| go xs
    go []        = Nil

anyMap :: (forall a. f a -> b) -> AnyList f -> [b]
anyMap f (x :| xs) = f x : anyMap f xs
anyMap _ Nil       = []

anyFoldr :: (forall a. f a -> b -> b) -> AnyList f -> b -> b
anyFoldr f (x :| xs) z = x `f` anyFoldr f xs z
anyFoldr _ Nil       z = z

anyFoldl' :: (forall a. b -> f a -> b) -> AnyList f -> b -> b
anyFoldl' f (x :| xs) z
  = let !z' = f z x
    in
    anyFoldl' f xs z'
anyFoldl' _ Nil       z = z

anyMapM :: Monad m => (forall a. f a -> m b) -> AnyList f -> m [b]
anyMapM f xs = sequence (anyMap f xs)

anyMapM_ :: Monad m => (forall a. f a -> m b) -> AnyList f -> m ()
anyMapM_ f xs = liftM (const ()) (anyMapM f xs)


{-
-- Polykinded Functor instance and associated stuff (Heavily based on the
-- `hask` package).
data Nat (p :: i -> i -> *) (q :: j -> j -> *) (f :: i -> j) (g :: i -> j) where
  Nat :: forall p q f g. forall a. Ob p a => q (f a) (g a) -> Nat p q f g

class Category p where
  type Ob p :: i -> Constraint
  id :: Ob p a => p a a
  (.) :: p b c -> p a b -> p a c

class Vacuous (c :: i -> i -> *) (a :: i)
instance Vacuous c a

instance Category (->) where
  type Ob (->) = Vacuous (->)
  id = Prelude.id
  (.) = (Prelude..)

instance Category (Nat k k) where

class (Category (Dom f), Category (Cod f)) => Functor (f :: i -> j) where
  type Dom f :: i -> i -> *
  type Cod f :: j -> j -> *

  fmap :: Dom f a b -> Cod f (f a) (f b)

instance Functor (Either a) where
  type Dom (Either a) = (->)
  type Cod (Either a) = (->)

  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

instance Functor Either where
  type Dom Either = (->)
  type Cod Either = Nat (->) (->)

data Nat' f g = Nat' (forall a. f a -> g a)

instance Category Nat' where
  -- ...

instance Functor Existential where
  type Dom Existential = Nat'
  type Cod Existential = (->)

  fmap (Nat' f) (Existential x) = Existential (f x)

instance Functor AnyList where
  type Dom AnyList = Nat'
  type Cod AnyList = (->)

  fmap n@(Nat' f) (x :| xs) = f x :| fmap n xs
  fmap _          Nil       = Nil
-}
