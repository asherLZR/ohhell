module Parser where

import Prelude
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Traversable

import Instances

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed UnexpectedEof) "abc")
-- True
failed :: ParseError -> Parser a
failed e = P (\_ -> Error e)
-- failed e = P {parse=(\_ -> Error e)}

-- | Produces a parser that always fails with @UnexpectedChar@ using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser c = failed (UnexpectedChar c)

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character :: Parser Char
character = P func
    where 
        func :: Input -> ParseResult Char
        func "" = Error $ UnexpectedString ""
        func (x:xs) = Result xs x

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof :: Parser ()
eof = P func
    where
        func :: Input -> ParseResult ()
        func "" = Result "" ()
        func x = Error $ ExpectedEof x

-- | Return a parser that tries the first parser for a successful value, then:
--
--   * if the first parser succeeds then use this parser; or
--
--   * if the first parser fails, try the second parser.
--
-- >>> parse (character ||| pure 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed UnexpectedEof ||| pure 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| pure 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed UnexpectedEof ||| pure 'v') "abc"
-- Result >abc< 'v'
(|||) :: Parser a -> Parser a -> Parser a
(|||) p q = P (\i -> if isErrorResult(p' i) == False then p' i else parse q i)
    where 
        p' i = parse p i

infixl 3 |||

-- | Return a parser that continues producing a list of values from the given
-- parser.
--
-- /Tip:/ Use @list1@, @pure@ and @(|||)@.
--
-- >>> parse (list character) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> pure 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> pure 'v')) ""
-- Result >< ""
list :: Parser a -> Parser [a]
list p = list1 p ||| P(\i -> Result i [])

-- parse p :: Input -> Result a

-- | Return a parser that produces at least one value from the given parser
-- then continues producing a list of values from the given parser (to
-- ultimately produce a non-empty list).
--
-- /Tip:/ Use /bind/ @(>>=)@, @list@ and @pure@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> pure 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> pure 'v')) "")
-- True

lift :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift f a b = f <$> a <*> b

list1 :: Parser a -> Parser [a]
list1 p = lift (:) p (list p)

-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- /Tip:/ Use /bind/ @(>>=)@, @unexpectedCharParser@ and @character@.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P func
    where
        func a = if a /= "" && f (head a) then parse character a else parse (unexpectedCharParser $ head a) ""

-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
--
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> isErrorResult (parse (is 'c') "")
-- True
--
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is c = satisfy (\i -> if i == c then True else False)

-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
--
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> isErrorResult (parse (isNot 'c') "")
-- True
--
-- >>> isErrorResult (parse (isNot 'c') "c")
-- True
isNot :: Char -> Parser Char
isNot c = satisfy (\i -> if i /= c then True else False)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @isDigit@ functions.
digit :: Parser Char
digit = satisfy isDigit

--
-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @isSpace@ functions.
space :: Parser Char
space = satisfy isSpace

-- | Write a parser that will parse zero or more spaces.
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces :: Parser String
spaces = list space

-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
spaces1 :: Parser String
spaces1 = list1 space

-- | Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @isLower@ functions.
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @isUpper@ functions.
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @isAlpha@ functions.
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Return a parser that sequences the given list of parsers by producing all
-- their results but fails on the first failing parser of the list.
--
-- /Tip:/ Use monads and @pure@.
--
-- /Tip:/ Optionally use @foldr@. If not, an explicit recursive call.
--
-- We want any character, followed by lower case 'x', then any upper case
-- letter.
-- >>> seq = [character, is 'x', upper]
-- >>> parse (sequenceParser seq) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser seq) "abCdef")
-- True
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = sequence

-- | Return a parser that produces the given number of values off the given
-- parser.  This parser fails if the given parser fails in the attempt to
-- produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany :: Int -> Parser a -> Parser [a]
thisMany i p = sequenceParser $ replicate i p

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- /Tip:/ Use the monad instance.
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok p = do 
    a <- p
    _ <- spaces
    pure a

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
--
-- /Tip:/ Use @tok@ and @is@.
charTok :: Char -> Parser Char
charTok c = is c

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
--
-- /Tip:/ Use @charTok@.
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that parses the given string (fails otherwise).
--
-- /Tip:/ Use @is@ and @traverse@.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string s = traverse is s

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Tip:/ Use @tok@ and @string@.
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok s = do
    a <- string s
    _ <- spaces
    pure a

-- | Write a function that produces a non-empty list of values coming off the
-- given parser (which must succeed at least once), separated by the second
-- given parser.
--
-- /Tip:/ Use @list@ and the monad instance.
--
-- /Tip:/ Use anonymous apply @(*>)@ to ignore the results of a parser.
--
-- >>> parse (sepby1 character (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby1 character (is ',')) "a,b,c"
-- Result >< "abc"
--
-- >>> parse (sepby1 character (is ',')) "a,b,c,,def"
-- Result >def< "abc,"
--
-- >>> isErrorResult (parse (sepby1 character (is ',')) "")
-- True
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p q = do
    p' <- p
    pq' <- list(q *> p)
    pure(p':pq')

-- | Write a function that produces a list of values coming off the given
-- parser, separated by the second given parser.
--
-- /Tip:/ Use @sepby1@ and @|||@.
--
-- >>> parse (sepby character (is ',')) ""
-- Result >< ""
--
-- >>> parse (sepby character (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby character (is ',')) "a,b,c"
-- Result >< "abc"
--
-- Result >def< "abc,"
sepby :: Parser a -> Parser s -> Parser [a]
sepby p q = sepby1 p q ||| pure []

-- | Write a function that parses one of the characters in the given string.
--
-- /Tip:/ Use @satisfy@ and @elem@.
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof s = satisfy (`elem` s)

-- | Write a function that parses any character, but fails if it is in the
-- given string.
--
-- /Tip:/ Use @satisfy@ and @notElem@.
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof s = satisfy (`notElem` s)

-- | Write a function that applies the first parser, runs the third parser
-- keeping the result, then runs the second parser and produces the obtained
-- result.
--
-- /Tip:/ Use the monad instance.
--
-- >>> parse (between (is '[') (is ']') character) "[a]"
-- Result >< 'a'
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "abc]")
-- True
between :: Parser o -> Parser c -> Parser a -> Parser a
between o c a = do
  _ <- o
  oa' <- a
  _ <- c
  pure(oa')

-- | Write a function that applies the given parser in between the two given
-- characters.
--
-- /Tip:/ Use @between@ and @charTok@.
--
-- >>> parse (betweenCharTok '[' ']' character) "[a]"
-- Result >< 'a'
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
-- True
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok c1 c2 p = do
  between (charTok c1) (charTok c2) p
