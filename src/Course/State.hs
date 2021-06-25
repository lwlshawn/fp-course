{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec state seed = snd $ runState state seed

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval state seed = fst $ runState state seed

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\s -> (s,s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put seed = State (\_ -> ((), seed))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f state = State (\seed -> case runState state seed of (a,b) -> (f a, b))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure val = State (\seed -> (val,seed))
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b 
  (<*>) s1 s2 = State (\seed -> let (f,s) = runState s1 seed; 
                                    (a,s') = runState s2 s in  
                                    (f a, s'))

  --State (\seed -> (fst $ runState s1 seed) (fst $ runState s2 seed))
  -- how are we supposed to combine the resulting states? i have no guarantee on what the type
  -- of s will be. in their given example, the states ["apple"] and ["banana"] combine up
  -- ah i get it, i probably take the state after the first expression, and use that for
  -- the second expression. that would be the most intuitive way of doing it.

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) f s = State (\seed -> let (a,s') = runState s seed in 
                                  (runState (f a)) s')
{-
f $ exec s seed produces State s b instead of (b,s) this is tricky.
State s b, means that State contains a function that has type signature :: s -> (b,s)

the problem is that the unwrapping has to be done in the "future" so to speak
first i get a seed, i use that seed to produce (a,s) then i want to use this a 
as a value to feed into f to get State s b

one point of view is that i try to produce a function that gives me a (b,s) but that
seems to be hard. So i instead thought of just producing State s b straight, but
i cant do that without the seed value to let me run State s a, thats my issue. 
even if i get a seed value to run State s a, what am i then supposed to do to
"run" and get (b,s)?

the way i implemented this is as follows; given a seed, i run the initial State s a,
to give me a (a, s'), then i use a to create a State s b, into which i feed s' as the 
new seed to finally get (b,s'') as the final result. 
-}



-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)

findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = return Empty
findM pr (t :. xs) = (pr t) >>= (\b -> case b of True -> return (Full t)
                                                 False -> findM pr xs)


-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3

pred' :: Ord a => a -> State (S.Set a) Bool
pred' ele = State (\set -> (S.member ele set, S.insert ele set))

firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat ls = fst (runState (findM pred' ls) S.empty)


{-
time to trace types. findM :: (a -> f Bool) -> List a -> f (Optional a)
the "state" we want to track is the items seen so far, kept inside a set, so i can't imagine
f not being State S.Set. Lets substitute that in first

findM :: (a -> State S.Set Bool) -> List a -> State S.Set (Optional a)
how does findM work? The key line is as follows:

findM pr (t :. xs) = (pr t) >>= (\b -> case b of True -> return (Full t)
                                                 False -> findM pr xs)...

i think i have the intuition, and it let me code something that typechecks.

I want to try and reason through what happens here.

the pred' I defined above takes an element
and returns true if its in the "previous" set, false otherwise and adds
the current element to the set. handling of state is done by findM
that "threads" the state through using >>= for us?


what actually happens when we call (findM pred' (1 :. 2 :. 1 :. Nil))? lets trace

- findM _ Nil fails, so we go to next pattern
- findM pr (1 :. (2 :. 1 :. Nil)) = (pr 1) >>= (\b -> case b of True -> return (Full t)
                                                                False -> findM pr xs))

pr = pred', so (pred' 1) = State (\set -> (S.member 1 set, S.insert 1 set))
at this point i think haskell has to leave it and can't do anything further? Since without
providing an explicit starting set, its impossible to evaluate case b of ...

-- COME BACK TO THIS
-}

-- some basic practice with the State monad i did to help complete the task above
--LEVEL 1: Add things to a set
push' :: Ord a => a -> State (S.Set a) ()
push' ele = State $ \set -> ((), S.insert ele set)

checkandpush :: Ord a => a -> State (S.Set a) Bool
checkandpush ele = State $ \set -> (S.member ele set, S.insert ele set)

--LEVEL 2: Add three ints to a set using State
-- addthree :: State (S.Set Int) ()
-- addthree = do
--   push' 2
--   push' 3
--   push' 4

--LEVEL 3: Add a list of Ints to a set using State
addList :: List Int -> State (S.Set Int) ()
addList (h :. t) = do
  push' h >> addList t
addList Nil = return ()


-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct ls = fst $ runState (filtering pred'' ls) S.empty

pred'' :: Ord a => a -> State (S.Set a) Bool
pred'' ele = State (\set -> (not $ S.member ele set, S.insert ele set))

{-
actually the simplest way is to just convert a list to a set, and convert back
but i'll not do it that way since clearly they want you to work with State.

filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
again, i can only assume that f has to be State (S.set a), so lets substitute

filtering :: Applicative f => (a -> State (S.Set a) Bool) -> List a -> State (S.Set a) (List a)
pred is the same as before, but you keep an element when the predicate is true, so
you keep the element if it is NOT already a member of the set. If you've seen it before, discard it
-}



-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy = (contains 1) . firstRepeat . (produce sumSquareOfDigits)

sumSquareOfDigits :: Integer -> Integer
sumSquareOfDigits = toInteger . sum . map (\x -> x * x) . map (digitToInt) . listh . show

-- find first element that repeats
-- firstRepeat ::
--   Ord a =>
--   List a
--   -> Optional a
-- firstRepeat ls = fst (runState (findM pred' ls) S.empty)

-- digitToInt :: Char -> Int
-- contains :: Eq a => a -> Optional a -> Bool

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
-- produce :: (a -> a) -> a -> List a


-- | Flattens a combined structure to a single structure.
-- join :: Monad f => f (f a) -> f a

{-
sumSquareOfDigits :: Int -> Int
sumSquareOfDigits = sum . map (\x -> x * x) . map (digitToInt) . show

given an integer, make an infinite list, where next element is computed using sumSquareOfDigits
then find the firstRepeat element, and check if its 1

-}