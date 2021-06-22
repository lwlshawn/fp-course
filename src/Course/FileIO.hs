{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.
  
Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- getArgs :: IO (List Chars) -- returns a list of program command line arguments not inc. program name
-- putStrLn :: Chars -> IO () -- duh
-- readFile :: FilePath -> IO Chars -- converts a file to single continuous string (Chars == List Char synonym)
-- lines :: Chars -> List Chars -- breaks a single continuous string into lines demarcated by \n i assume
-- void :: IO a -> IO () -- discards or ignores the result of evaluation, such as the return of an IO action

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath -- List Char synonym
  -> Chars
  -> IO ()
printFile name contents = do
  putStrLn name
  putStrLn contents

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles Nil = return ()
printFiles ((n,c) :. xs) = printFile n c >> printFiles xs

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path = (\x -> (path,x)) <$> readFile path

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles ls = sequence $ map getFile ls  -- map getFile ls :: List IO (FilePath, Chars) 
-- sequence converts that to IO (List (FilePath, Chars))

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run path = getFile path >>= (\(_,files) -> getFiles $ lines files) >>= printFiles
--getFile path :: IO (FilePath, Chars)
--(\(_,files) -> getFiles $ lines files) :: (FilePath, Chars) -> IO (List (FilePath, Chars))


-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = (join <$> getArgs) >>= run
-- getArgs IO (List Chars) which is IO (List (List Char))
----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
