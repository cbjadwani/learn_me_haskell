module SParser where

import Control.Monad
import Char
import Control.Applicative

infixl 7 ?
infixl 6 #
infixl 6 -#
infixl 6 #-
infixl 5 >->

type Parsed a   = Maybe a
type ParseST a  = (String, Parsed a)
newtype Parser a   = P {parse :: String -> ParseST a}

instance Monad Parser where
    return x    = P $ \inp -> (inp, Just x)
    p >>= f     = P $ \inp -> case parse p inp of
                        (_, Nothing) -> (inp, Nothing)
                        (inp', Just x) -> parse (f x) inp'
    fail _      = empty

instance Functor Parser where
    fmap f p    = P $ \inp -> case parse p inp of
                        (_, Nothing) -> (inp, Nothing)
                        (inp', Just x) -> (inp', Just $ f x)

instance Applicative Parser where
    pure        = return
    pf <*> p    = P $ \inp -> case parse pf inp of
                        (inp', Just f) -> parse (p >-> f) inp'
                        (_, Nothing) -> (inp, Nothing)

instance Alternative Parser where
    m <|> n     = P $ \inp -> case parse m inp of
                        (inp', Nothing) -> parse n inp
                        x -> x
    empty       = P $ \_ -> ([], Nothing)

manyn :: Int -> Parser a -> Parser [a]
manyn           = replicateM

(?) :: Parser a -> (a -> Bool) -> Parser a
m ? p           = do
                    v <- m
                    if p v
                        then return v
                        else fail ""
--
(#) :: Parser a -> Parser b -> Parser (a, b)
m # n           = do
                    j <- m
                    k <- n
                    return (j, k)
--
(>->) :: Parser a -> (a -> b) -> Parser b
m >-> f         = fmap f m
--
(-#) :: String -> Parser a -> Parser a
s -# p  = accept s *> p

(#-) :: Parser a -> String -> Parser a
p #- s  = p <* accept s

char :: Parser Char
char            = P $ \cs -> case cs of
                        [] -> ([], Nothing)
                        (c:cs') -> (cs', Just c)

lit :: Char -> Parser Char
lit             = token . lit'
lit' c          = char ? (==c)

alpha, alphanum, num, space :: Parser Char
alpha           = char ? Char.isAlpha
num             = char ? Char.isDigit
alphanum        = char ? Char.isAlphaNum
space           = char ? Char.isSpace

token :: Parser a -> Parser a
token m         = m <* (many space)

--

digitVal, nat, number :: Parser Int
digitVal        = num >-> digitToInt
nat             = token $ foldl1 (\a n -> a*10 + n) <$> some digitVal
number          = ("-" -# nat >-> (0-) ) <|> nat

--
accept :: String -> Parser String
accept          = token . sequence . map lit'

-----------------------------------------------------------------

data RE a       = Empty 
                | Singleton a
                | Catenation (RE a) (RE a)
                | Alternation (RE a) (RE a)
                | Kleene (RE a)
                deriving Show

type StringRE   = RE Char

sngl :: Parser StringRE
sngl        = Singleton <$> char ? (`notElem` "()*|")

kleene' :: StringRE -> Parser StringRE
kleene' e   = "*" -# return (Kleene e) <|> return e

unit'       = "(" -# re #- ")" <|> sngl
unit        = unit' >>= kleene'

units       = (foldr1 Catenation) <$> some unit

altOp       = "|" -# return Alternation
re          = units <**> altOp <*> re <|> units
