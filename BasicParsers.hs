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
-- Test: runParser letter (Stream "abc123")
letter :: Parser Char
letter = foldl (<|>) empty charArr
    where
        letterArr = ['a'..'z'] ++ ['A'..'Z']
        charArr   = map char letterArr

-- FP2.1
-- Test: runParser dig (Stream "123abc")
dig :: Parser Char
dig = foldl (<|>) empty charArr
    where
        digArr  = ['0'..'9']
        charArr = map char digArr

-- FP2.2
-- Test: runParser between ((char 'a') (char 'b') (char 'c')) (Stream "abc")
-- betweenEx = runParser (between ((char 'a') (char 'b') (char 'c'))) (Stream "abc")
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

-- FP2.2
-- Test: runparser whitespace (char 'a') (Stream " a b ")
whitespace :: Parser a -> Parser a
whitespace p = between (many x) p (many x)
    where x = char ' ' <|> char '\n'

-- FP2.3
-- "1,2,3,4" -> ["1234", Stream ""]
-- "1,2,,3,4" -> ["12", Stream ",3,4"]
-- "1,22,3,4" -> ["12234", Stream ""]
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p1 p2 = fmap (concat) x
    where 
        x = many ((some p1) <* p2)
-- sep1 p1 p2 = many (p1 <* p2)
-- sep1 p1 p2 = (:) <$> p1 <*> many ((many p2) *> p1)

-- FP2.3
sep :: Parser a -> Parser b -> Parser [a]
sep p1 p2 = many ((many p2) *> p1)

-- FP2.3
option :: Parser a -> Parser a -> Parser a
option x p = p <|> x

-- FP2.4
string :: String -> Parser String
string "" = pure ""
string (x:xs) = (:) <$> (char x) <*> (string xs)

-- FP2.4
identifier :: Parser String
identifier = whitespace ((:)<$> lower <*> (many (lower <|> dig)))
    where
        lletter = ['a'..'z']
        charArr = map (char) lletter
        lower   = foldl (<|>) empty charArr