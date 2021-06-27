{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focused position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

lefts ::
  ListZipper a
  -> List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a
  -> List a
rights (ListZipper _ _ r) =
  r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  (<$>) f (ListZipper ls a rs) = ListZipper (map f ls) (f a) (map f rs)

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  (<$>) _ IsNotZ = IsNotZ
  (<$>) f (IsZ lszip) = IsZ $ (f <$> lszip)

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList ::
  ListZipper a
  -> List a
-- toList (ListZipper ls a rs) = (reverse ls) ++ (a :. Nil) ++ rs
toList (ListZipper ls a rs) = f ls (a :. rs) where
  f Nil xs = xs 
  f (h :. t) xs = f t (h :. xs)


-- | Convert the given (maybe) zipper back to a list.
toListZ ::
  MaybeListZipper a
  -> List a
toListZ IsNotZ = Nil
toListZ (IsZ z) = toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList ::
  List a
  -> MaybeListZipper a
fromList Nil = IsNotZ
fromList (h :. t) = IsZ $ ListZipper Nil h t

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional ::
  MaybeListZipper a
  -> Optional (ListZipper a)
toOptional IsNotZ = Empty
toOptional (IsZ lszip) = Full lszip

zipper ::
  [a]
  -> a
  -> [a]
  -> ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional ::
  Optional (ListZipper a)
  -> MaybeListZipper a
fromOptional Empty =
  IsNotZ
fromOptional (Full z) =
  IsZ z

asZipper ::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asZipper f =
  asMaybeZipper (IsZ . f)

(>$>)::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asMaybeZipper _ IsNotZ =
  IsNotZ
asMaybeZipper f (IsZ z) =
  f z

(-<<) ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(-<<) =
  asMaybeZipper

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus ::
  (a -> a)
  -> ListZipper a
  -> ListZipper a
withFocus f (ListZipper ls fcs rs) = ListZipper ls (f fcs) rs

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus ::
  a
  -> ListZipper a
  -> ListZipper a
setFocus fcs (ListZipper ls _ rs) = ListZipper ls fcs rs

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper a
  -> a
  -> ListZipper a
(.=) =
  flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft ::
  ListZipper a
  -> Bool
hasLeft (ListZipper ls _ _) = case ls of Nil -> False
                                         _   -> True

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight ::
  ListZipper a
  -> Bool
hasRight (ListZipper _ _ rs) = case rs of Nil -> False
                                          _   -> True 

-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == IsNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5]) -- [1,2,1,3,4,5]
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]
findLeft ::
  (a -> Bool)
  -> ListZipper a
  -> MaybeListZipper a
findLeft _ (ListZipper Nil _ _) = IsNotZ
findLeft pred (ListZipper ls fcs rs)
  = let (bf, aft) = break pred ls in 
    case aft of Nil    -> IsNotZ --fails
                (h:.t) -> IsZ (ListZipper t h (toList (ListZipper bf fcs rs))) 

{-
everytime i try to do it purely in my head, i regret it. PLAN FIRST

ok so when i search the left list, treating it like a "normal list" works
because i'm searching "away" from the focus. so when i use break, break splits
into (bf, aft) where aft begins with the first element that satisfies
the predicate. if aft is empty, this is a sign that break failed to
find anything. 

if aft is not empty, the first element of aft is the new focus
and the tail of aft is the new left list of the zipper.

the new right list of the zipper is the combination of the current focus,
the before from break, and the current right list. So we call toList on

ListZipper bf fcs rs. Could also have used reverse but did it this way 
on principle for efficiency.
-}



-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == IsNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight ::
  (a -> Bool)
  -> ListZipper a
  -> MaybeListZipper a
findRight pred (ListZipper lls fcs rls)
  = let (bf, aft) = break pred rls in
    case aft of Nil    -> IsNotZ 
                (h:.t) -> IsZ (ListZipper ((reverse bf) ++ (fcs :. lls)) h t)

{-
ok now the search is on the right, i we break with pred on right list

(bf, aft) = break pred rls
again, is aft is empty, it means break failed

if aft is not empty, then the first element of aft is the element that
matched the predicate, and becomes the new focus.

the tail of aft is now the new right list.

this means that bf, the old focus, and the old left list should combine to
form the new left list. Just used reverse to keep the code simple this time.
-}



-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop ::
  ListZipper a
  -> ListZipper a
moveLeftLoop (ListZipper Nil fcs rs) = let (h:.t) = reverse (fcs :. rs) in (ListZipper t h Nil) 
moveLeftLoop (ListZipper (h:.t) fcs rs) = ListZipper t h (fcs :. rs)


-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop ::
  ListZipper a
  -> ListZipper a
moveRightLoop (ListZipper ls fcs Nil) = let (h:.t) = reverse (fcs :. ls) in (ListZipper Nil h t)
moveRightLoop (ListZipper ls fcs (h:.t)) = ListZipper (fcs :. ls) h t

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft ::
  ListZipper a
  -> MaybeListZipper a
moveLeft (ListZipper Nil _ _) = IsNotZ
moveLeft (ListZipper (h:.t) fcs rs) = IsZ $ ListZipper t h (fcs:.rs)

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight ::
  ListZipper a
  -> MaybeListZipper a
moveRight (ListZipper _ _ Nil) = IsNotZ
moveRight (ListZipper ls fcs (h:.t)) = IsZ $ ListZipper (fcs :. ls) h t

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft ::
  ListZipper a
  -> MaybeListZipper a
swapLeft (ListZipper Nil _ _) = IsNotZ
swapLeft (ListZipper (h:.t) fcs rs) = IsZ $ ListZipper (fcs:.t) h rs


-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight ::
  ListZipper a
  -> MaybeListZipper a
swapRight (ListZipper _ _ Nil) = IsNotZ
swapRight (ListZipper ls fcs (h:.t)) = IsZ $ ListZipper ls h (fcs:.t)

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts ::
  ListZipper a
  -> ListZipper a
dropLefts (ListZipper _ fcs rs) = ListZipper Nil fcs rs

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights ::
  ListZipper a
  -> ListZipper a
dropRights (ListZipper ls fcs _) = ListZipper ls fcs Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN ::
  Int
  -> ListZipper a
  -> MaybeListZipper a 
moveLeftN n zipl@(ListZipper ls fcs rs)
  | n < 0         = moveRightN (-n) zipl
  | length ls < n = IsNotZ
  | n == 0        = IsZ zipl
  | otherwise     = let (h:.t) = (reverse (take n ls)) ++ (fcs:.rs) in IsZ $ ListZipper (drop n ls) h t 

-- take n ls 

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]

-- moveRightN 2 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]

moveRightN ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
moveRightN n zipl@(ListZipper ls fcs rs)
  | n < 0         = moveLeftN (-n) zipl
  | length rs < n = IsNotZ
  | n == 0        = IsZ zipl
  | otherwise     = let (h:._) = drop (n-1) rs in IsZ $ ListZipper ((take (n-1) rs) ++ (fcs:.ls)) h (drop (n) rs)

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
moveLeftN' ::
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
moveLeftN' n zipls@(ListZipper ls _ _)
  | n < 0 = moveRightN' (-n) zipls
  | length ls < n = Left (length ls)
  | otherwise = let (IsZ movedZipLs) = moveLeftN n zipls in Right movedZipLs

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' ::
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
moveRightN' n zipls@(ListZipper _ _ rs)
  | n < 0 = moveLeftN' (-n) zipls
  | length rs < n = Left (length rs)
  | otherwise = let (IsZ movedZipLs) = moveRightN n zipls in Right movedZipLs


-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
nth n zipls
  | n < 0 = nth (-n) zipls
  | n >= length ls' = IsNotZ
  | otherwise = IsZ $ ListZipper (reverse (take (n) ls')) h (drop (n+1) ls')
  where
    ls' = toList zipls
    (h:._) = drop n ls'

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z z' -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index ::
  ListZipper a
  -> Int
index (ListZipper ls _ _) = length ls

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end ::
  ListZipper a
  -> ListZipper a
end zipls = case moveRight zipls of IsNotZ -> zipls
                                    (IsZ ls') -> end ls'

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start ::
  ListZipper a
  -> ListZipper a
start zipls@(ListZipper ls _ _) = movedZipLs where
  (IsZ movedZipLs) = moveLeftN (length ls) zipls

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft ::
  ListZipper a
  -> MaybeListZipper a
deletePullLeft (ListZipper Nil _ _) = IsNotZ
deletePullLeft (ListZipper (h:.t) _ rs) = IsZ $ ListZipper t h rs

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight ::
  ListZipper a
  -> MaybeListZipper a
deletePullRight (ListZipper _ _ Nil) = IsNotZ
deletePullRight (ListZipper ls _ (h:.t)) = IsZ $ ListZipper ls h t

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft ::
  a
  -> ListZipper a
  -> ListZipper a
insertPushLeft newf (ListZipper ls f rs) = ListZipper (f:.ls) newf rs

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight ::
  a
  -> ListZipper a
  -> ListZipper a
insertPushRight newf (ListZipper ls f rs) = ListZipper ls newf (f:.rs)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Applicative ListZipper where
-- /Tip:/ Use @List#repeat@.
  pure a = ListZipper (repeat a) a (repeat a)
-- /Tip:/ Use `zipWith`
  (<*>) (ListZipper ls1 fs1 rs1) (ListZipper ls2 fs2 rs2)
    = ListZipper ls3 fs3 rs3
    where
      ls3 = zipWith (\f a -> f a) ls1 ls2
      fs3 = fs1 fs2
      rs3 = zipWith (\f a -> f a) rs1 rs2

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> IsNotZ <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsNotZ
-- ><
--
-- >>> IsNotZ <*> IsNotZ
-- ><
instance Applicative MaybeListZipper where
  pure a = IsZ (pure a)
  (<*>) IsNotZ _ = IsNotZ
  (<*>) _ IsNotZ = IsNotZ
  (<*>) (IsZ zipl1) (IsZ zipl2) = IsZ (zipl1 <*> zipl2)

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5], [] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
{-
here its a ListZipper of ListZippers,
left list has the ListZippers
[1] >2< [3,4,5]
[] >1< [2,3,4,5]

right list has the ListZippers
[3,2,1] >4< [5]
[4,3,2,1] >5< []

focus is
[2,1] >3< [4,5]
-}
instance Extend ListZipper where
  (<<=) f zipls= ListZipper (unfoldr (\zipl -> case moveLeft zipl of IsNotZ          -> Empty
                                                                     (IsZ movedLeft) -> Full (f movedLeft, movedLeft)) zipls)
                            (f zipls)
                            (unfoldr (\zipl -> case moveRight zipl of IsNotZ           -> Empty
                                                                      (IsZ movedRight) -> Full (f movedRight, movedRight)) zipls)


{-
(<<=) :: (f a -> b) -> f a -> f b, specialised to ListZipper, we have
(<<=) :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
the idea is that given a ListZipper, there are many different forms of it that are the "same" ListZipper
so we want to take the function and apply to all of them, and collect all the results in a ListZipper.

unfoldr :: (a -> Optional (b, a)) -> a -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

hint is to use unfoldr. If i take it literally, i would suppose
i use it to produce the left and right list. 

The function i'm given as an argument has type (ListZipper a -> b)
going from unfoldr's signature i want it to produce the "next" element
by moving left, or right respectively i guess? I think i have the high level idea in place.

Irritatingly, we have a MaybeListZipper that didn't just use optional (honestly why not??)
directly, so I have to unwrap and rewrap between two essentially isomorphic types.
-}



-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= IsNotZ
-- ><
--
-- >>> id <<= (IsZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend MaybeListZipper where
  (<<=) _ IsNotZ = IsNotZ
  (<<=) f (IsZ zipls) = IsZ $ (f . IsZ) <<= zipls

{-
(<<=) :: (f a -> b) -> f a -> f b, specialised to MaybeListZipper, we have
(<<=) :: (MaybeListZipper a -> b) MaybeListZipper a -> MaybeListZipper b

...how? I CAN"T BELIEVE THIS WORKED HAHAHAHAHAHAHAH. I didn't know how to 
"downgrade" the signature of f from (MaybeListZipper a -> b) down to
(ListZipper a -> b) so i just extended the pipe, internally i made a function
that goes ListZipper a -> MaybeListZipper a -> b, so externally it looks like
ListZipper a -> b and i can use the extend implemented for ListZipper. 

I'm actually not 100 percent sure if this is sound, but it type checks and
solves their testcases. Come back to this in the future as well.
-}



-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure (ListZipper _ fs _) = fs

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
instance Traversable ListZipper where
  traverse f (ListZipper ls fs rs) = ListZipper <$> (traverse f ls) <*> (f fs) <*> (traverse f rs)

{-
traverse :: Applicative f => (a -> f b) -> t a -> f (t b), specialising,
traverse :: Applicative f => (a -> f b) -> ListZipper a -> f (ListZipper b)

so if i try to call the list instance of traverse:
traverse-ls :: Applicative f => (a -> f b) -> List a -> f (List b)

Ah, then i just lift ListZipper into the applicative context.
-}




-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id IsNotZ
-- ><
--
-- >>> traverse id (IsZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance Traversable MaybeListZipper where
  traverse _ IsNotZ = pure IsNotZ
  traverse f (IsZ zipl) = IsZ <$> (traverse f zipl)

{-
traverse :: Applicative f => (a -> f b) -> t a -> f (t b), specialising,
traverse-M :: Applicative f => (a -> f b) -> MaybeListZipper a -> f (MaybeListZipper b)
traverse-Z :: Applicative f => (a -> f b) -> ListZipper a -> f (ListZipper b)

so (traverse-Z f zipl) :: f (ListZipper b)
then i just map IsZ inside.
-}

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"
