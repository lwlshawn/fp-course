{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Data.Char
import qualified Prelude as P
import Course.Traversable

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789" 
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b" 
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13" 
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07" 
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars input = dollars ++ (listh "and ") ++ cents where
  (rawDollars, rawCents) = break (\c -> c == '.') input
  dollars = printDollars $ clean rawDollars
  cents = printCents $ clean (drop 1 rawCents)

clean :: Chars -> Chars 
clean input = 
  filter (\x -> x /= ' ') (map repl input)
  where
    repl c = case (not $ isNumber c) of True -> ' '
                                        False -> c

printCents :: Chars -> Chars
printCents s 
  | s == (listh $ "01") = "one cent"
  | length s == 0 = "zero cents"
  | length s > 2 = printCents (take 2 s)
  | length s == 2 = case (traverse (fromChar) s) of Empty -> error "traverse in printCents length 2 broke"
                                                    (Full (d1:.d2:.Nil)) -> (listh $ show $ D2 d1 d2) ++ (listh " cents")
  | length s == 1 = case (traverse (fromChar) s) of Empty -> error "traverse in printCents length 1 broke"
                                                    (Full (d3 :. Nil)) -> (listh $ show $ D2 d3 Zero) ++ (listh " cents")

-- pulls correct list of suffixes i need to "zip" with d3s.
pullSuffixes :: Chars -> List Chars
pullSuffixes s 
  | length s < 4 = ("dollars" :. Nil)
  | length s >= 4 = reverse ls' where 
    (h:.t) = take ((div ((length s) - 1) 3) + 1) illion 
    ls' = (listh "dollars" :. t)

segment3 :: Chars -> List Chars
segment3 (a:.b:.c:.t) = (a:.b:.c:.Nil) :. segment3 t 
segment3 Nil = Nil 
segment3 _ = error "segment3 broke" 
--obviously using errors like this are unideal but i just want to get this to work.

-- takes in a length 3 string, converts to digit3
stringToD3 :: Chars -> Digit3
stringToD3 s
  | length s == 3 = case (traverse (fromChar) s) of Empty -> error "stringToD3 broke in traverse"
                                                    (Full (d1:.d2:.d3:.Nil)) -> D3 d1 d2 d3
  | otherwise = error "stringToD3 broke"




-- i should have done this from the start, instead of instancing show
-- since i already instanced show, just using this to make it easier and work
-- with chars everywhere.
showD3 :: Digit3 -> Chars
showD3 d3 = listh $ show d3 

printDollars :: Chars -> Chars
printDollars Nil = "zero dollars "
printDollars ('0':.Nil) = "zero dollars "
printDollars ('1':.Nil) = "one dollar "
printDollars s  
  | (length s) `mod` 3 == 0 = let
                                 zippedList = zipWith zipFunc lsd3s suffixes
                                 suffixes = pullSuffixes s
                                 lsd3s = map stringToD3 (segment3 $ trunc s)
                              in foldRight (\a acc -> a ++ acc) Nil zippedList
  -- | otherwise = error "shouldnt be here"
  | otherwise = let
                    zippedList = zipWith zipFunc lsd3s suffixes
                    suffixes = pullSuffixes s
                    processedFstGrp = fstGroup (take ((length s) `mod` 3) s) 
                    lsd3s    = processedFstGrp :. (map stringToD3 (segment3 $ trunc s)) -- process the truncated segment, and then pass it here
                in foldRight (\a acc -> a ++ acc) Nil zippedList   


zipFunc :: Digit3 -> List Char -> List Char
zipFunc (D1 Zero) _ = Nil 
zipFunc (D2 Zero Zero) _ = Nil
zipFunc (D3 Zero Zero Zero) _ = Nil
zipFunc a b = (showD3 a) ++ (listh " ") ++ b  ++ (listh " ")

-- printDollars s = 
--   foldRight (\a acc -> a ++ acc) Nil zippedList
--   where
--     zippedList = zipWith (\a b -> (showD3 a) ++ (listh " ") ++ b ++ (listh " and ") ) lsd3s suffixes
--     suffixes = pullSuffixes s
--     processedFstGrp = fstGroup (take ((length s) `mod` 3) s) 
--     lsd3s    = processedFstGrp :. (map stringToD3 (segment3 $ trunc s)) -- process the truncated segment, and then pass it here


-- truncates string to a length where it is divisible by 3.
trunc :: Chars -> Chars
trunc s = drop ((length s) `mod` 3) s 


--really hacky, very hardcoded, very ugly way of dealing with the first group of three.
fstGroup :: Chars -> Digit3
fstGroup s@(c1:.c2:.Nil) = case (traverse (fromChar) s) of Empty -> error "fstgroup 2 digit broke"
                                                           (Full (d1:.d2:.Nil)) -> D2 d1 d2
fstGroup (c1:.Nil) = case (fromChar c1) of Empty -> error "fstgroup 1 digit broke"
                                           (Full d1) -> D1 d1
fstGroup _ = error "fstgrp broke"


instance Show Digit3 where
  show (D1 d) = hlist $ showDigit d
  show (D2 d1 d2) = case d1 of Zero -> hlist $ showDigit d2
                               One  -> showTeen (d1,d2)
                               o -> case d2 of Zero -> showTy d1
                                               _    -> showTy d1 P.++ "-" P.++ (hlist $showDigit d2)
  show (D3 d1 d2 d3) = case (d1,d2,d3) of (Zero,Zero,Zero) -> "zero"
                                          _                -> case (d2,d3) of (Zero,Zero) -> (hlist $ showDigit d1) P.++ " hundred"
                                                                              _           -> case d1 of Zero -> show (D2 d2 d3)
                                                                                                        _    -> (hlist $ showDigit d1) P.++ " hundred and " P.++ show (D2 d2 d3)




showTeen :: (Digit,Digit) -> [Char]
showTeen (d1,d2) = case (d1,d2) of
  (One, Zero)   -> "ten" 
  (One, One)    -> "eleven"
  (One, Two)    -> "twelve"
  (One, Three)  -> "thirteen"
  (One, Four)   -> "fourteen"
  (One, Five)   -> "fifteen"
  (One, Six)    -> "sixteen"
  (One, Seven)  -> "seventeen"
  (One, Eight)  -> "eighteen"
  (One, Nine)   -> "nineteen"


showTy :: Digit -> [Char]
showTy d = case d of
  One     -> "ten"
  Two     -> "twenty"
  Three   -> "thirty"
  Four    -> "forty"
  Five    -> "fifty"
  Six     -> "sixty"
  Seven   -> "seventy"
  Eight   -> "eighty"
  Nine    -> "ninety"
  _       -> error "showTy broke"