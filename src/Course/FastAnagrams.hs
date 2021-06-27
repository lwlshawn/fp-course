{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S
import Course.Applicative
import Course.Monad

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str path =
  do
    file <- readFile path
    let dict = foldLeft (\s e -> S.insert e s) S.empty (lines file)
      in return (filter (\x -> S.member x dict) (permutations str)) 

fastAnagrams1 ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams1 name f =
  (flip (filter . flip S.member) (permutations name) . S.fromList . hlist . lines) <$> readFile f

-- final edit: looked up solution out of curiosity, runs at basically the same speed as mine.

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

{-
no guidance whatsoever is provided, aside from the import
set so my guess is just that
a set is fater then running intersect by on naive lists.

plan is to just load the entire dictionary into a set, then
run membership checks on the set, and just hope this is
faster.

seems dubious since you still incur the initial linear 
runtime from inserting the dictionary into the set, absolutely
unavoidable since you have to read the entire dictionary at least once

so bigO wise it doesn't appear to change complexity, unless the word you are
anagramming is really large, which also doesn't make sense.

doesn't really make any significant difference for a four letter word.
 Too bad i guess.
-}