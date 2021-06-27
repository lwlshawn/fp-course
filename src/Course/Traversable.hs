{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

{-
traverse :: (a -> f b) -> List a -> f (List b)
he defines it by taking in a function f' :: (a -> f b)
and then does foldRight on the incoming list, with a function

(\a b -> (:.) <$> f a <*> b) (pure Nil)
foldRight :: (a -> b -> b) -> b -> List a -> b
so here b is the 'accumulated value', he uses pure Nil as "base value"
which is like putting an empty list inside the functor, makes sense.

now the function:
\a b -> (:.) <$> f' a <*> b 
given an incoming element a, first i apply the function f' to it.
this gives me (f b), and the accumulated value is (f List b)
so i simply lift concatenation into the applicative context. Makes sense.
-}

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse f (ExactlyOne a) = ExactlyOne <$> f a


instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full a) = Full <$> f a

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60

sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA = traverse id 

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- given a traversable structure containing (f a), i want to make it into
-- an applicative containing (t a). In some sense, i'm collecting results into
-- the structure, then re-wrapping it into the applicative. 

-- if t is traversable, i can call traverse, which is likely the idea here
-- in this case i want the type to be (a -> f a) -> t a -> f (t b)?

-- no but my incoming structure is t (f a) not t a. traverse takes a function
-- that takes an element of the structure, and produces an applicative of that element

-- so it is like doing some operation over every element of t, with some effect
-- captured by the applicative?

-- traverse :: (f a -> f a) -> t (f a) -> f (t a) is what i want.
-- i.e. the original a = f a, original b = a, because elements of my traversable are already f a.


instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse h (Compose t) = Compose <$> traverse (traverse h) t


{-
lets go through this carefully:

class Functor t => Traversable t where
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

Here, we are trying to instance Traversable (Compose f g). So outermost level, we have
traverse :: Applicative f0 => (a -> f0 b) -> (Compose f g) a -> f0 (Compose f g) b

If i reach inside Compose f g, i obtain a variable of type f (g a). 
Now, f is itself traversable, so i can call traverse on it as follows:

traverse-f :: Applicative f1 => (a -> f1 b) -> (f a) -> f1 (f b).
Keeping the big picture goal in mind, i want a = (g a) and b = (g b). Substituting,

traverse-f :: Applicative f1 => ((g a) -> f1 (g b)) -> (f (g a)) -> f1 (f (g b))
kind of my issue is that the outermost level traverse, has a function (a -> f0 b) which
does not match (g a) -> f1 (g b). {on reflection, should have noticed this looked like the 
result of traversing g if i'd paid closer attention.}

lets go one more level down and see if focusing on g gives me some insight.
if i treat g as the traversable function, and consider how i might call traverse on g:

traverse-g :: Applicative f0 => (a -> f0 b) -> g a -> f0 (g b)
in this case i can actually use the original function that was provided to outermost traverse

hang on. look at the tail end of what i just wrote. g a -> f0 (g b). This is
what i needed for traverse-f!

so if i call h, the function given to outermost traverse, then
(traverse-g h) :: g a -> f0 (g b).

Going up one level, if i feed this into traverse-f, it becomes
traverse-f (traverse-g h) :: (f (g a)) -> f0 (f (g b))

now i can just fmap Compose inside the applicative! :) lets give this a roll.

i can't trace the code exactly, but intuitively it makes sense if i imagine f as a big structure, where
each "cell" of f is itself a structure g, that contains a. so mapping traverse (traverse h) is like

"for each tree (g) in forest (f)". The part that is a bit lost to me is how the f0 travels outwards
so seamlessly. The rough idea i had in my mind thought i'd run into f0 (f (f0 (g ( a)))) type of problems.
the type signatures make sense of course, but i should come back to this in the future. 
-}



-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) h (Product fa ga) = Product (h <$> fa) (h <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse h (Product tf tg) = Product <$> (traverse h tf) <*> (traverse h tg)

{-
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

Intuitively, given traversable f and g, i just traverse them individually,
and somehow "collect" results at the end. First the specialised sig is:

traverse :: Applicative f0 => (a -> f0 b) -> (Product f g) a -> f0 ((Product f g) b)
If i try to do this individually, and treat f/g as the traversable, i get:

traverse :: Applicative f0 => (a -> f0 b) -> f/g a -> f0 (f/g b)

if i have f0 (f b) and f0 (g b) i can just lift Product in to get
Product <$> (f0 (f b)) <*> (f0 (g b)) = f0 (Product (f b) (g b)). first try!
-}



-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) f (InL fa) = InL $ f <$> fa
  (<$>) f (InR ga) = InR $ f <$> ga

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse f (InL fa) = InL <$> traverse f fa
  traverse f (InR ga) = InR <$> traverse f ga

-- this typechecked and i just guessed the solution but I should just do a sanity check anyway
-- traverse :: Applicative f0 => (a -> f0 b) -> t a -> f (t b). Specialised, we get
-- traverse :: Applicative f0 => (a -> f0 b) -> (Coproduct f g) a -> f0 ((Coproduct f g) b)

-- specialising to either f or g;
-- traverse :: Applicative f0 => (a -> f0 b) -> f a -> f0 (f b). And then rewrap. makes sense.