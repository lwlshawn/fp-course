{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.JsonParser where

import Course.Core
import Course.Parser
import Course.MoreParser
import Course.JsonValue
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
import Data.Ratio

-- $setup
-- >>> :set -XOverloadedStrings

-- A special character is one of the following:
-- * \b  Backspace (ascii code 08)
-- * \f  Form feed (ascii code 0C)
-- * \n  New line
-- * \r  Carriage return
-- * \t  Tab
-- * \v  Vertical tab
-- * \'  Apostrophe or single quote (only valid in single quoted json strings)
-- * \"  Double quote (only valid in double quoted json strings)
-- * \\  Backslash character
data SpecialCharacter =
  BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving (Eq, Ord, Show)

-- NOTE: This is not inverse to @toSpecialCharacter@.
fromSpecialCharacter ::
  SpecialCharacter
  -> Char
fromSpecialCharacter BackSpace =
  chr 0x08
fromSpecialCharacter FormFeed =
  chr 0x0C
fromSpecialCharacter NewLine =
  '\n'
fromSpecialCharacter CarriageReturn =
  '\r'
fromSpecialCharacter Tab =
  '\t'
fromSpecialCharacter VerticalTab =
  '\v'
fromSpecialCharacter SingleQuote =
  '\''
fromSpecialCharacter DoubleQuote =
  '"'
fromSpecialCharacter Backslash =
  '\\'

-- NOTE: This is not inverse to @fromSpecialCharacter@.
toSpecialCharacter ::
  Char
  -> Optional SpecialCharacter
toSpecialCharacter c =
  let table = ('b', BackSpace) :.
              ('f', FormFeed) :.
              ('n', NewLine) :.
              ('r', CarriageReturn) :.
              ('t', Tab) :.
              ('v', VerticalTab) :.
              ('\'', SingleQuote) :.
              ('"' , DoubleQuote) :.
              ('\\', Backslash) :.
              Nil
  in snd <$> find ((==) c . fst) table
  
-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal characters. See http://json.org for the full list of control characters in JSON.
--
-- /Tip:/ Use `hex`, `fromSpecialCharacter`, `between`, `is`, `charTok`, `toSpecialCharacter`.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString ::
  Parser Chars
jsonString = between (is '"') (is '"') (flatten <$> (list1 jsonString'))

jsonString' :: Parser Chars
jsonString' =   
  controlChar 
  ||| block
  ||| hexu'

block :: Parser Chars
block = list1 (noneof ('\\' :. '"' :. Nil))

controlChar :: Parser Chars
controlChar  = 
  is '\\' 
  >> character 
  >>= (\c -> case toSpecialCharacter c of Empty -> unexpectedCharParser c
                                          (Full c') -> valueParser $ (fromSpecialCharacter c' :. Nil) )

hexu' :: Parser Chars --upgraded hexu for purpose here.
hexu' = 
  is '\\'
  >> ((:. Nil) <$> hexu)

{-

-- block = list (noneof ('\\' :. '"' :. Nil))
-- block is the one thats broken. 
-- parse block "\\abcd" eats a slash

Found the heart of the problem. The following crashes:

parse (flatten <$> (list1 block)) "\\abcd"
actually, it crashes on ANY input. 

but

parse block "\\abcd" does not.
The first call shouldn't even do anything, so it crashing is concerning. Lets trace.


parse (flatten <$> (list1 block)) "\\abcd"

parse (flatten <$> (list1 block)) pulls out a function, that we feed "\\abcd" into
so what i need to do is analyse the structure of this function.


(flatten <$> (list1 block))

first, lets analyse (list1 block)

(list1 block) = (:.) <$> block <*> (list block)
block = list (satisfy isAlpha), so

list1 block = (:.) <$> (list (satisfy isAlpha)) <*> (list block)
.. but (list block) = (list1 block) ||| pure Nil 

list1 block = (:.) <$> (list (satisfy isAlpha)) <*> (list1 block ||| pure Nil)
... so just from this, it IS possible that it recurses downward forever.


So it looks like the problem might have to do with the fact that i'm using list
on a parser that itself uses list. We did this in the CIS194 course as well
without issues however. 

i had some insight about why the recursion doesn't normally break a program.
list tries (list1 p) and sees if it "fails". list1 p calls p at least once,
so if p fails properly there's no issue.. AH.

i just thought of a problem. if list1 calls a p that never fails, it will loop
forever...and this fixed it. It was literally just using list1 instead of list in block.

45 minutes i'll never get back. The problem is that list1 was using a parser that
never fails, but also didn't consume any input, so it internally bounced between
list1 and list without consuming anything from the string.
-}



-- | Parse a JSON rational.
--
-- /Tip:/ Use @readFloats@.
-- again, not providing approxRational in the hint
-- is highly triggering. Some of these hints are so
-- shoddily done.

-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber ::
  Parser Rational
jsonNumber =
  P (\s -> case readFloats s of Empty -> UnexpectedString s
                                Full (n, r) -> Result r (approxRational n 0.00001))

-- | Parse a JSON true literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue ::
  Parser Chars
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse ::
  Parser Chars
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull ::
  Parser Chars
jsonNull = stringTok "null"

-- | Parse a JSON array.
--
-- /Tip:/ Use `betweenSepbyComma` and `jsonValue`.
-- 
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray ::
  Parser (List JsonValue)
jsonArray = betweenSepbyComma '[' ']' jsonValue


-- | Parse a JSON object.
--
-- /Tip:/ Use `jsonString`, `charTok`, `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject ::
  Parser Assoc
jsonObject = betweenSepbyComma '{' '}'
  (
    list1 space >> pure Nil  ||| 
    jsonString >>= (\k -> spaces >> (charTok (':')) >> jsonValue >>= (\v -> pure (k,v)))
  )
-- list space should be wrong, but it doesnt fail the way i expect.
-- instead of looping forever it finds unexpected char '\' which is
-- really strange, given that i'm using the ||| notation. If the first
-- parser never fails, how is it giving me unexpected char?  

-- got it. Its because of how between works; the parser that fails
-- is the parser that tries to process the comma, and i am correct to say
-- that list space >> pure Nil doesn't fail on any inpuit. 


test :: Parser (List Chars)
test = betweenSepbyComma '{' '}' jsonString

test2 :: Parser (Chars, JsonValue)
test2 = jsonString >>= (\k -> (charTok (':') >> jsonValue >>= (\v -> pure (k,v))))

-- | Parse a JSON value.
--
-- /Tip:/ Use `spaces`, `jsonNull`, `jsonTrue`, `jsonFalse`, `jsonArray`, `jsonString`, `jsonObject` and `jsonNumber`.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue ::
  Parser JsonValue
jsonValue =
  (JsonString <$> jsonString)   |||
  (jsonTrue >> pure JsonTrue)   |||
  (jsonFalse >> pure JsonFalse) |||
  (jsonNull >> pure JsonNull)   |||
  (JsonRational <$> jsonNumber) |||
  (JsonArray <$> jsonArray)     |||
  (JsonObject <$> jsonObject)

-- | Read a file into a JSON value.
--
-- /Tip:/ Use @System.IO#readFile@ and `jsonValue`.
readJsonValue ::
  FilePath
  -> IO (ParseResult JsonValue)
readJsonValue path = (parse jsonValue) <$> readFile path -- :: IO String
