{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`

{-
This says g bind (f bind x) isomorphic ((g bind) . f) bind x

On the left, g is a monadic function, f is a monadic function, and x
is a value. f =<< x produces a monadic value, that is then
fed into g.

On the right, ((g bind) . f) is the function fed into =<< it takes in
the value x, and applies f x, before applying (g bind) to f x. Makes sense.

The main issue with the law is that its not clear when at all this would fail 
to hold.

-}


class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) f (ExactlyOne x) = f x

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) f xs = flatten $ map f xs

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) _ Empty = Empty
  (=<<) f (Full x) = f x

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) ::
    (a -> ((->) t b)) -- a -> t -> b
    -> ((->) t a)     -- t -> a
    -> ((->) t b)     -- t -> b
  (=<<) f g x = f (g x) x
-- expected type is what HASKELL expects
-- actual type is what i've provided

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::
  Monad f =>
  f (a -> b)
  -> f a
  -> f b
(<**>) m a = (\f -> f <$> a) =<< m
{-
m is a monad, containing a function of type (a -> b)
when i use =<< with m as the right argument, =<< "pulls out" (a -> b) 
and binds it to f. so f now has type a -> b

a above is a monad of type a, so now i can fmap f over it to get a monad of type b. 
-}

infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Monad f =>
  f (f a)
  -> f a
join m = id =<< m 

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.

-- this is wrong. Bind as recognised by haskell is >>=, =<< is the flipped version.

--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Monad f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) m f = join $ f <$> m

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
(<=<) f g a = f =<< (g a)

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
