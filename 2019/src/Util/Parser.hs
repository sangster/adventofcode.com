--
-- See http://dev.stephendiehl.com/fun/002_parsers.html
--
module Util.Parser
    ( Parser(..)
    , runParser
    , satisfy
    , char
    , string
    , digit
    , natural
    , number
    , token
    , reserved
    , spaces
    , oneOf
    , chainl
    , chainl1
    ) where


import Data.Bool
import Data.Char
import Control.Monad
import Control.Applicative


newtype Parser a = Parser { parse :: String -> [(a, String)] }


runParser :: Parser a
          -> String
          -> a
runParser m s =
    case parse m s of
        [(res, [])] -> res
        [(_,   rs)] -> error "did not read entire string"
        _           -> error "failure"


instance Functor Parser where
    fmap f (Parser cs) = Parser cs'
      where cs' s = [(f a, b) | (a, b) <- cs s]


instance Applicative Parser where
    pure = return

    (Parser cs1) <*> (Parser cs2) = Parser cs
      where cs s = [(f a, s2) | (f, s1) <- cs1 s,
                                (a, s2) <- cs2 s1]


instance Monad Parser where
    return = unit
    (>>=)  = bind


bind :: Parser a
     -> (a -> Parser b)
     -> Parser b
bind p f = Parser cs
  where cs s      = concatMap f' $ parse p s
        f' (a, s) = parse (f a) s


unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])


instance MonadPlus Parser where
    mzero = failure
    mplus = combine


failure :: Parser a
failure = Parser (\_ -> [])


combine :: Parser a
        -> Parser a
        -> Parser a
combine p q = Parser cs
  where cs s = (parse p s) ++ (parse q s)


instance Alternative Parser where
    empty = mzero
    (<|>) = option


option :: Parser a
       -> Parser a
       -> Parser a
option p q = Parser cs
  where cs s = case parse p s of
                   []  -> parse q s
                   res -> res


satisfy :: (Char -> Bool)
        -> Parser Char
satisfy p = do { c <- item; bool failure (unit c) (p c) }


item :: Parser Char
item = Parser cs
  where cs []      = []
        cs (c:cs') = [(c, cs')]


char :: Char
     -> Parser Char
char c = satisfy (c ==)


string :: String
       -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)


digit :: Parser Char
digit = satisfy isDigit


natural :: (Integral a, Read a)
        => Parser a
natural = read <$> some (satisfy isDigit)


number :: Parser Int
number = do s  <- string "-" <|> return []
            cs <- some digit
            return . read $ s ++ cs


token :: Parser a
      -> Parser a
token p = do a <- p
             spaces
             return a


reserved :: String
         -> Parser String
reserved = token . string


spaces :: Parser String
spaces = many $ oneOf " \n\r"


oneOf :: String
      -> Parser Char
oneOf = satisfy . (flip elem)


chainl :: Parser a
       -> Parser (a -> a -> a)
       -> a
       -> Parser a
chainl p op a = chainl1 p op <|> return a


chainl1 :: Parser a
        -> Parser (a -> a -> a)
        -> Parser a
chainl1 p op = do { a <- p; rest a <|> return a }
  where rest a = do f <- op
                    b <- p
                    rest (f a b)
