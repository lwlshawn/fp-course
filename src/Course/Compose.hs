{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose composite) = Compose $ (f <$>) <$> composite
-- f :: a -> b
-- composite :: f0 (g a)
-- f0 is a functor, containing (g a) so if i want to fmap over it
-- i need a function that has type (g a) -> (g b)
-- fmap:: (a -> b) -> g a -> g b
-- so fmap f has type (g a) -> (g b)


instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
-- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose h) (Compose a) = Compose $ (<*>) <$> h <*> a 

-- what am i doing here? first i fmap <*> over h
-- h :: f (g (a -> b))
-- <*> :: g (a -> b) -> g a -> g b

-- so fmapping it inside, i get that
-- h <*> f becomes f (g (a -> b) <*>) which now can be combined with
-- a f (g a) so that <*> has the correct arguments to produce a g b.


instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
