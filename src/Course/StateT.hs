{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
-- in words: if f is a Functor, then (StateT s f) is a Functor where fmap is implemented as follows
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a -- this does NOT mean that StateT contains a functor parameterised by a
    -> StateT s f b
  (<$>) f (StateT g) = StateT (\state -> (\(a,s) -> (f a, s)) <$> (g state))
-- to add on to my notes about how filtering works below, notice that when we do f <$> (StateT g)
-- we essentially add f to the "future" of (StateT g); we create a new function that adds f to the pipeline
-- of things that should happen in the future, when we eventually get a state in which to perform
-- the stateful computation. 

-- (\(a,s) -> (f a, s)) :: (a, s) -> (b, s)
-- g state :: functor (a, s)
-- the fmap is doing ((a, s) -> (b,s)) -> functor (a, s) -> functor (b, s)
-- essentially a = (a,s) and b = (b,s) in this context.


-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a = StateT (\state -> return (a, state)) -- valid because f is a monad.
  (<*>) ::
   StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) (StateT f) (StateT g) = StateT (\state -> do 
                                                    (h, newState) <- f state
                                                    (a, finalState) <- g newState
                                                    return (h a, finalState)) 



-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  (=<<) f (StateT g) = StateT (\state -> do
                                            (a, newState) <- g state
                                            runStateT (f a) newState)
--(\state -> g state >>= (\(a, newState) -> runStateT (f a) newState))


-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT (\state -> ExactlyOne $ f state)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' c s = runExactlyOne $ runStateT c s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT c s = snd <$> (runStateT c s)

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' c s = snd $ (runState' c s)

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT c s = fst <$> runStateT c s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' c s = fst $ (runState' c s)

-- | A `StateT` where the state also distributes into the produced value.
-- StateT s f a = (runStateT :: s -> f (a, s))
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT (\s -> pure (s, s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative f =>
  s
  -> StateT s f ()
putT s = StateT (\_ -> pure ((), s))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
-- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
-- ok so now f is a State' (S.Set a), translated we get
-- filtering :: (a -> State' (S.Set a) Bool) -> List a -> State' (S.set a) (List a) 

-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' = (`eval'` S.empty) . filtering (\ele -> state' (\set -> (not $ S.member ele set, S.insert ele set)))


-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
-- Optional is a monad, so StateT S.Set Optional is an applicative, then the type of filtering is:
-- filtering :: (a -> StateT S.Set Optional Bool) -> List a -> StateT S.Set Optional (List a)

-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF = (`evalT` S.empty) . filtering (\ele -> StateT (\set -> case ele > 100 of True -> Empty
                                                                                     False -> Full (not $ S.member ele set, S.insert ele set)))
{-
what confuses me is why filtering works even though StateT is just an applicative and not a monad
(\ele -> StateT (\set -> case ele > 100 of True -> Empty
                                           False -> Full (not $ S.member ele set, S.insert ele set)))
(a -> StateT S.Set Optional Bool), so yes, this does fit the type signature. 

the main issue is that the way filtering is implemented feels disjointed; like the applicative f isn't
threaded through properly, and indeed i am NOT using bind, so why on earth does it still work?

ok so in words, my "predicate" function that i use to filter elements does the following.
- it takes an element called ele, and it returns a StateT with a function which does the following
- the function takes in a set, checks if the element is too big, if it is it ends, otherwise
- it gives a Full (Bool, newSet), True if the element is distinct, false otherwise, newSet inserting element
- the way filtering works, it maps this bool to ALL the elements, so at this point, i have a list of type
- List (StateT S.Set Optional Bool) since StateT S.Set Optional is my applicative in this case. 

- now with this list of List (StateT S.Set Optional Bool) and original List a, i zip them with helper
- in the filtering function.

- the end result of helper is StateT S.Set Optional (List a).. ok at this point i can roughly follow
- the reson it works is because how helper concatenates the "one element lists" are done using 
- the applicative interface of StateT S.Set Optional, which is the mechanism that allows the state
- to pass through correctly.
-}



-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) h (OptionalT g) = OptionalT $ (h <$>) <$> g 
-- h :: a -> b , g :: f (Optional a) where f is a functor 
-- what i need is a function that has type :: (Optional a) -> (Optional b)
-- the more readable translation of what i've done above is OptionalT $ fmap (fmap h) g
-- the inner (fmap h) is a function that has type (Optional a) -> (Optional b)
-- this is the perfect type to operate on g that has type f (Optional a), so i use
-- an outermost fmap to call it on g. the point is that the innermost (fmap h) relies on
-- the fact that Optional is a functor, and the outermost fmap relies on the fact that g
-- is a functor. 


-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure a = OptionalT $ return $ Full a
  (<*>) (OptionalT fOpG) (OptionalT fOpA) = OptionalT $ 
                                              fOpG >>= (\opG -> case opG of Empty  -> return Empty
                                                                            Full g -> fOpA >>= (\opA -> onFull (\t -> return (Full (g t))) opA))


  (<*>) (OptionalT fOpG) (OptionalT fOpA) = OptionalT $ do
                                              opG <- fOpG 
                                              opA <- fOpA
                                              case opG of Empty  -> return Empty
                                                          Full g -> onFull (\t -> return (Full (g t))) opA

-- ok so this implementation is buggy right now. Fails the fifth and sixth cases above. I think its
-- something to do with the Empty clause where i return Empty. OptionalT f a represents
-- a data structure containing one thing, a functor f, wrapping a Optional a.

-- (<*>) :: f (a -> b) -> f a -> f b
-- in this case, f is (OptionalT f') 
-- (<*>) :: OptionalT f' (a -> b) -> OptionalT f' a -> OptionalT f' b

--onFull notes:
-- | takes a function (t -> f (Optional a)) and an Optional t, produces f (Optional a)
-- | i see, if Optional T is Full, it uses the function, otherwise it just returns (f Empty)
-- onFull :: Applicative f => (t -> f (Optional a)) -> Optional t -> f (Optional a)


-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
-- (=<<) :: Monad f => (a -> f b) -> (f a) -> (f b)
-- (>>=) :: Monad f => (f a) -> (a -> f b) -> (f b)
instance Monad f => Monad (OptionalT f) where
  (=<<) g (OptionalT fOpA) = OptionalT $ fOpA >>= (\opA -> case opA of Empty -> return Empty
                                                                       Full a -> runOptionalT (g a))

-- fOpA :: f (Optional a)
-- fOpA >>= (\OpA -> 
-- need to produce a f (Optional b) here)

-- g :: a -> OptionalT f b
-- fmap g :: (Optional a) -> (Optional b)
-- fmap (fmap g) :: f (Optional a) -> f (Optional b)
-- opT :: OptionalT f a, 

-- (=<<) g (OptionalT fOpA) = fOpA >>= (\opA -> opA >>= (\a -> g a))  
-- m :: OptionalT f' a 
-- ok here i think i at least understand where the error is coming from. 
-- fOpA :: f (Optional a) and is the left argument to the outermost >>=, so the right hand side
-- should be a function that produces a f (Optional b)
--now going inside, opA :: Optional a, >>= thus expects a function that takes in a, produces Optional b
--this is why this fails, because g a produces a OptionalT f b instead.



-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) f (Logger ls a) = Logger ls (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure a = Logger Nil a
  (<*>) (Logger xs f) (Logger ys a) = Logger (xs ++ ys) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) f (Logger xs a) = case (f a) of (Logger ys fa) -> Logger (xs ++ ys) fa

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l a = Logger (l :. Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG = filterLog
            . runOptionalT
            . (`evalT` S.empty)
            . filtering (\ele -> StateT (\set -> OptionalT 
            $ case ele > 100 of True  -> log1 (listh "aborting > 100: " ++ listh (show ele)) $ Empty
                                False -> case (mod ele 2) of 0 -> log1 (listh "even number: " ++ listh (show ele)) $ Full (not $ S.member ele set, S.insert ele set)
                                                             _ -> log1 Nil $ Full (not $ S.member ele set, S.insert ele set)))


filterLog :: Logger Chars (Optional (List a)) -> Logger Chars (Optional (List a))
filterLog (Logger ls o) = Logger (filt ls) o where
  filt Nil = Nil
  filt (t :. xs)
    | t == "" = filt xs
    | otherwise = t :. (filt xs)

{-
This solution is truly horrible. I got it by just "brute forcing" types to figure out 
where things should go but the way i use filterlog at the end is super hacky, because 
log1 Nil added a "" into the array, instead of leaving the log unchanged. 

The open ended things I need to come back to to understand in the future:

1. I expected to have to do some conversion; because i thought my solution gave a 
Logger Chars (Optional (List a, S.Set)) at the end, but instead this already
correctly returns a Logger Chars (Optional (List a)) and i dont know why.

-- ok so using the statements below, i was able to confirm the "definite" type
of the various monads i use in distinctG. and indeed the Logger has type

Logger Chars (Optional ([Int], S.Set Int)) so i'm not sure how this becomes
Logger Chars (Optional (List Int)). the conversion from ([Int], S.Set Int) -> List Int
is happening without me having to do it, and i'm not sure why.

lets slowly trace upwards starting from filtering.
filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)

filtering needs to take in a function a -> f Bool. In this case the monad we give it is
StateT (S.Set Int) (OptionalT (Logger Chars)) I believe.
-}

myF :: (Integral a, Show a) => a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
myF = (\ele -> StateT (\set -> OptionalT 
            $ case ele > 100 of True  -> log1 (listh "aborting > 100: " ++ listh (show ele)) $ Empty
                                False -> case (mod ele 2) of 0 -> log1 (listh "even number: " ++ listh (show ele)) $ Full (not $ S.member ele set, S.insert ele set)
                                                             _ -> log1 Nil $ Full (not $ S.member ele set, S.insert ele set)))

{-
Ok. So this typechecks. (we just use Int instead of the general a).
This means that the type signature of filtering specialised to this situation is:

filtering :: (a -> StateT (S.Set Int) (OptionalT (Logger Chars) Bool)) -> List Int -> StateT (S.Set Int) (OptionalT (Logger Chars)) (List Int) 
evalT :: StateT s f a -> s -> f a -- takes starting state, then pulls out f a. In this case, after we call evalT S.empty we should have

OptionalT (Logger Chars) (List Integer)?
-}

a1 :: (Integral a, Show a) => List a -> StateT (S.Set a) (OptionalT (Logger Chars)) (List a)
a1 = (filtering myF)

myG :: (Integral a, Show a) => OptionalT (Logger Chars) (List a)
myG = (`evalT` S.empty) . (filtering myF) $ listh [1,2,3]

{-
Wait. why is this different from below where bar still has type
bar :: OptionalT (Logger Chars) ([Int], S.Set Int)? This is the point at which the change has
already happened and I don't understand why. AH. Its because of evalT! evalT removes the over
arching StateT wrapper and throws state away, retrieving just f a

in this case f a is OptionalT (Logger Chars) (List a). So we've solved our first problem.
-}


{-
2. I absolutely need to fix this incredibly hacky way of dealing with the ""
that i couldn't get rid of.

To solve this problem, I need to understand how myF works better. In reality
i'm not sure why my log is coming out correctly... OH. Its because of how
Logger l a is a pair of LIST l and a. So Logger (List Char) tells me that
my log is List (List Char), i.e. a List of Strings (List Char) = Chars = String.

This also answers my question. The reason log1 Nil is adding a "" is because
"" is interpreted as Chars, and added to List Chars. Now how can I redesign
myF to avoid this issue?

I just want to present the value without making any change to the log. What comes
to mind is using pure. Lets give that a roll
-}

myF' :: (Integral a, Show a) => a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
myF' = (\ele -> StateT (\set -> OptionalT 
            $ case ele > 100 of True  -> log1 (listh "aborting > 100: " ++ listh (show ele)) $ Empty
                                False -> case (mod ele 2) of 0 -> log1 (listh "even number: " ++ listh (show ele)) $ Full (not $ S.member ele set, S.insert ele set)
                                                             _ -> pure $ Full (not $ S.member ele set, S.insert ele set)))

-- it typechecked! Lets try putting this back into our original solution.

distinctG' ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG' = runOptionalT
            . (`evalT` S.empty)
            . filtering myF'

-- works beautifully :) 


-- checking types of things. 
foo :: StateT (S.Set Int) (OptionalT (Logger Chars)) [Int]
foo = return [1]

bar :: OptionalT (Logger Chars) ([Int], S.Set Int)
bar = runStateT foo S.empty

baz :: Logger Chars (Optional ([Int], S.Set Int))
baz = runOptionalT bar


-- | takes a function (t -> f (Optional a)) -> Optional t and produces f (Optional a)
-- | i see, if Optional T is Full, it uses the function, otherwise it just returns (f Empty)
onFull ::
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
