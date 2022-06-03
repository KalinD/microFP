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

data Parser r = P {
    runParser :: Stream -> [(r , Stream)]
}

instance Functor Parser where
    fmap f (P p) = P (\input -> [(f v, res) | (v, res) <- p input])    

instance Applicative Parser where
    pure p = (P (\input -> [(p, input)]))  
    (P p1) <*> (P p2) = P (\input -> [(f v, res2) | (f, res1) <- p1 input, (v, res2) <- p2 res1])

char :: Char -> Parser Char
char c = P p
    where
        p (Stream []) = []
        p (Stream (x:xs)) | c == x = [(x, Stream(xs))]
                          | otherwise = []

failure :: Parser a
failure = P (\input -> [])

instance Alternative Parser where
    empty = failure
    (P p1) <|> (P p2) = P (\input -> case p1 input of
            [] -> p2 input
            otherwise -> p1 input
        )
