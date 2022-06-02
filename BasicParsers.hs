-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Your Name (sxxxxxxx)
-- Student 2: Other Name (syyyyyyy)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb

letter ::Parser Char
letter = P (\(Stream (x:xs)) -> if x >= 'a' && x <= 'z' then [(x, Stream xs)] else [])
-- using foldl with Alternative
-- letter = foldl char [] ['a'..'z'] <$> char
-- fold (char) x xs
dig :: Parser Char
dig = P (\(Stream (x:xs)) -> if x >= '0' && x <= '9' then [(x, Stream xs)] else [])

between :: Parser a -> Parser b -> Parser c -> Parser B
between (P p1) (P p2) (P p3) = (P p2)