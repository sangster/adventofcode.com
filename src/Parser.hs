--
--
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- |
-- Module : Parser
--
-- A simple backtracking parser for intrepting puzzle input files.
--
-- See http://dev.stephendiehl.com/fun/002_parsers.html
module Parser
  (
    -- * Parser State
    Parser (..)
  , PState (..)

    -- * Execution
  , evalParser
  , parse

    -- * Utilities
  , eof
  , satisfy
  , oneOf
  , noneOf
  , chainl
  , chainl1
  , some
  , many
  , (<|>)
  , splitMany
  , splitSome

    -- * Simple types
  , item
  , char
  , digit
  , digit'
  , natural
  , number

    -- * Composite types
  , string
  , token
  , reserved
  , word
  , spaces

  , module Control.Monad.State.Strict
  ) where

import Data.Bool
import Data.Char
import Data.List (splitAt)
import Data.Default
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Except


newtype Parser a = Parser { runParser :: StateT PState (Either ParseError) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PState
    , MonadError ParseError
    )


data PState = PState
  { queue :: String
  , line  :: Int
  , col   :: Int
  }
  deriving Show


instance Default PState where
  def = PState
    { queue = ""
    , line  = 0
    , col   = 0
    }


data ParseError = UnexpectedToken Char Int Int
                | EOF
    deriving Show


evalParser :: Parser a
          -> String
          -> Either ParseError a
evalParser p s = evalStateT (runParser p) def{ queue = s }


parse :: forall a. Show a
      => Parser a
      -> String
      -> a
parse p s = report $ evalParser p s
  where
    report :: Either ParseError a -> a
    report (Right a) = a
    report (Left e)  = do
      error . unlines $
        [ "did not read entire string."
        , "    result: " ++ (prettyError e)
        , ""
        , " remaining: " ++ s
        ]

    prettyError (UnexpectedToken x l c) =
        unlines . concat $ [[err], before, [arrow], after]
      where
        err = "Unexpected token '"++[x]++"' at "++show l++":"++show c++":\n===="
        (before, after) = splitAt (l+1) $ lines s
        arrow = reverse $ '^' : take (c-1) (repeat '-')
    prettyError e = show e



unit :: a -> Parser a
unit = pure


instance Alternative Parser where
  empty = throwError EOF
  (<|>) = option


option :: Parser a
       -> Parser a
       -> Parser a
option p q = do
  st <- get
  case runStateT (runParser p) st of
    Left  _        -> q
    Right (a, st') -> put st' >> pure a


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- item
  if f x
    then unit x
    else do (PState _ l c) <- get
            throwError $ UnexpectedToken x l c


item :: Parser Char
item = get >>= cs . queue
  where
    cs []      = throwError EOF
    cs (c:cs') = do
      st <- get
      put st{ queue = cs' }
      putPos c
      pure c

    putPos :: Char -> Parser Char
    putPos c = do
      st <- get
      if c == '\n'
          then put st{ col = 0, line = (line st) + 1 }
          else put st{ col = (col st) + 1 }
      pure c


-- Assert that we've reached the end of the input
-- See https://en.wikipedia.org/wiki/End-of-Text_character
eof :: Parser Char
eof = get >>= cs
  where
    cs (PState [] _ _)    = unit '\ETX'
    cs (PState (x:_) l c) = throwError $ UnexpectedToken x l c


char :: Char -> Parser Char
char c = satisfy (c ==)


string :: String -> Parser String
string []     = pure []
string (c:cs) = char c >> string cs >> pure (c:cs)


digit :: Parser Char
digit = satisfy isDigit


digit' :: Parser Int
digit' = flip (-) (ord '0') . ord <$> digit


natural :: (Integral a, Read a) => Parser a
natural = read <$> some (satisfy isDigit)


number :: (Integral a, Read a) => Parser a
number = do
  s  <- string "-" <|> pure []
  cs <- some digit
  pure . read $ s ++ cs


token :: Parser a -> Parser a
token p = do { a <- p; spaces; pure a }


reserved :: String -> Parser String
reserved = token . string


word :: Parser String
word = some $ noneOf " \n\r"


spaces :: Parser String
spaces = many $ oneOf " \n\r"


oneOf :: String -> Parser Char
oneOf = satisfy . (flip elem)


noneOf :: String -> Parser Char
noneOf = satisfy . (not .) . flip elem


chainl :: Parser a
       -> Parser (a -> a -> a)
       -> a
       -> Parser a
chainl p op a = chainl1 p op <|> pure a


chainl1 :: Parser a
        -> Parser (a -> a -> a)
        -> Parser a
chainl1 p op = do { a <- p; rest a <|> pure a }
  where
    rest a = do { f <- op; b <- p; rest (f a b) }


splitMany :: Show b => Parser a -> Parser b -> Parser [b]
splitMany sep parser = splitSome sep parser <|> pure []


splitSome :: Show b => Parser a -> Parser b -> Parser [b]
splitSome sep parser = do
    p <- parser
    st <- get
    case runStateT (runParser sep) st of
      Left  _        -> pure [p]
      Right (_, st') -> do put st'
                           (:) <$> pure p <*> splitSome sep parser
