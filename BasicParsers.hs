-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Kalin Doychev (s2535645)
-- Student 2: Tariq Riahi (s2619393)
-- Student 3: Vincent Albertsson (s2247259)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb
import Debug.Trace

-- FP2.1
-- Parsers letter and dig which parse any alphabetical letter or any digit respectively
-- Test: runParser letter (Stream "abc123")
letter :: Parser Char
letter = foldl (<|>) empty charArr
    where
        letterArr = ['a'..'z'] ++ ['A'..'Z']
        charArr   = map char letterArr
letterEx1 = runParser letter (Stream "abc") 
letterEx2 = runParser letter (Stream "Abc")
letterEx3 = runParser letter (Stream "1abc")

-- Test: runParser dig (Stream "123abc")
dig :: Parser Char
dig = foldl (<|>) empty charArr
    where
        digArr  = ['0'..'9']
        charArr = map char digArr
digEx1 = runParser dig (Stream "0123")
digEx2 = runParser dig (Stream "1Abc")
digEx3 = runParser dig (Stream "abc")

-- FP2.2
-- Between parser which mimics the functionality of between in ParSec
-- Test: runParser (between (char '<') (char 'a') (char '>')) (Stream "<a>")
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3
betweenEx1 = runParser (between (char '<') (char 'a') (char '>')) (Stream "<a>")
betweenEx2 = runParser (between (char '<') (char 'a') (char '>')) (Stream "<b>")

-- Whitespace parser which receives a parser and uses it to parse the input while 
-- skipping spaces, newline, and tabulation
-- Test: runParser whitespace (char 'a') (Stream " a b ")
whitespace :: Parser a -> Parser a
whitespace p = between (many x) p (many x)
    where x = char ' ' <|> char '\n' <|> char '\t'
whitespaceEx1 = runParser (whitespace (char 'a')) (Stream " a b ")
whitespaceEx2 = runParser (whitespace (char 'a')) (Stream " ab ")
whitespaceEx3 = runParser (whitespace (char 'b')) (Stream " a b ")

-- FP2.3
-- "1,2,3,4" -> ["1234", Stream ""]
-- "1,2,,3,4" -> ["12", Stream ",3,4"]
-- "1,22,3,4" -> ["12234", Stream ""]
-- ""
-- The parser sep1 p s parses one or more occurrences of p, separated by one occurence of s.
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p1 p2 = fmap (concat) x
    where 
        x =(:) <$> (many p1) <*> many (p2 *> (some p1))
sep1Ex1 = runParser (sep1 dig (char ',')) (Stream "1,2,3,4,5")
sep1Ex2 = runParser (sep1 dig (char ',')) (Stream "12,34,5")
sep1Ex3 = runParser (sep1 dig (char ',')) (Stream "1,2,,3,4,5")

sep :: Parser a -> Parser b -> Parser [a]
sep p1 p2 = many ((many p2) *> p1)
sepEx1 = runParser (sep dig (char ',')) (Stream "1,,2,,3,,4,,5")
sepEx2 = runParser (sep dig (char ',')) (Stream "12,,,,,,,,,34,5")
sepEx3 = runParser (sep dig (char ',')) (Stream "1,2,;3,4,5")

option :: Parser a -> Parser a -> Parser a
option x p = p <|> x
optionEx1 = runParser (option dig letter) (Stream "abc1")
optionEx2 = runParser (option dig letter) (Stream "12abc")
optionEx3 = runParser (option dig letter) (Stream "!12abc")

-- FP2.4
-- Functions similarly to FP1.3, however works with a String rather than a Char
string :: String -> Parser String
string "" = pure ""
string (x:xs) = (:) <$> (char x) <*> (string xs)
stringEx1 = runParser (string "abc") (Stream "abc")
stringEx2 = runParser (string "abc") (Stream "abcdef")
stringEx3 = runParser (string "abc") (Stream "defabc")

-- Parses an identifier surrounded by whitespace
identifier :: Parser String
identifier = whitespace ((:)<$> lower <*> (many (lower <|> dig)))
    where
        lletter = ['a'..'z']
        charArr = map (char) lletter
        lower   = foldl (<|>) empty charArr
identifierEx1 = runParser identifier (Stream " a1 ")
identifierEx2 = runParser identifier (Stream " 1a ")
identifierEx3 = runParser identifier (Stream " A1 ")

-- Parses an integer surrounded by whitespace
integer :: Parser Integer
integer = whitespace (read <$> (some dig))
integerEx1 = runParser integer (Stream "1")
integerEx2 = runParser integer (Stream "10000")
integerEx3 = runParser integer (Stream "abc")

-- Parses a given string surrounded by whitespace
symbol :: String -> Parser String
symbol str = whitespace (string str)
symbolEx1 = runParser (symbol "!") (Stream " ! ")
symbolEx2 = runParser (symbol "!") (Stream " !?!")
symbolEx3 = runParser (symbol "!") (Stream " ?!")

-- Parses something with the provided parser surrounded by paranthesis
parens :: Parser a -> Parser a
parens p = between (char '(') p (char ')')
parensEx1 = runParser (parens dig) (Stream "(1)")
parensEx2 = runParser (parens dig) (Stream "{(1)}")

-- Parses something with the provided parser surrounded by braces
braces :: Parser a -> Parser a
braces p = between (char '{') p (char '}')
bracesEx1 = runParser (braces dig) (Stream "{1}")
bracesEx2 = runParser (braces dig) (Stream "({1})")