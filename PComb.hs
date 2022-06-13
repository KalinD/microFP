-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Kalin Doychev (s2535645)
-- Student 2: Tariq Riahi (s2619393)
-- Student 3: Vincent Albertsson (s2247259)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

-- FP1.1
-- Parser datatype which receives a stream and gives a result of type r
-- Test: :t char 'a'
data Parser r = P {
    runParser :: Stream -> [(r , Stream)]
}

-- FP1.2
-- Functor instance of Parser
parserFunctorEx = runParser (toUpper <$> char 'a') (Stream "abc")
instance Functor Parser where
    fmap f p = P (\input -> [(f v, res) | (v, res) <- runParser p input])

-- FP1.3
-- A simple parser that parses a single character
charParserEx = runParser (char 'a') (Stream "abc")
char :: Char -> Parser Char
char c = P p
    where
        p (Stream []) = []
        p (Stream (x:xs)) | c == x = [(x, Stream xs)]
                          | otherwise = []

-- FP1.4
-- A parser that fails and consumes no input
failureEx = runParser (failure) (Stream "abc")
failure :: Parser a
failure = P (\input -> [])

-- FP1.5
-- Applicative instance of Parser
parserCombEx = runParser ((,) <$> char 'a' <*> char 'b') (Stream "abc")
instance Applicative Parser where
    pure p = (P (\input -> [(p, input)]))  
    p1 <*> p2 = P (\input -> [(f v, res2) | (f, res1) <- runParser p1 input, (v, res2) <- runParser p2 res1])

-- FP1.6
-- Alternative instance of parser such that it tries as few 
-- alternatives as possible
altEx = runParser (char '1' <|> char 'a') (Stream "a1")
instance Alternative Parser where
    empty = failure
    p1 <|> p2 = P (\input -> case runParser p1 input of
            [] -> runParser p2 input
            res -> res
        )