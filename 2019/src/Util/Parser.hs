--
-- See http://dev.stephendiehl.com/fun/002_parsers.html
--
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Util.Parser
    ( Parser (..)
    , PState (..)
    , parse
    , satisfy
    , char
    , string
    , digit
    , digit'
    , natural
    , number
    , token
    , reserved
    , spaces
    , oneOf
    , chainl
    , chainl1
    , some
    , many
    , (<|>)
    , module Control.Monad.State
    ) where


import Data.Bool
import Data.Char
import Data.Default
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace


newtype Parser a = Parser { runParser :: StateT PState (Either ParseError) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState PState
             , MonadError ParseError
             )


data PState = PState { queue :: String
                     , line  :: Int
                     , col   :: Int
                     }
  deriving Show


instance Default PState where
    def = PState { queue = ""
                 , line  = 0
                 , col   = 0
                 }


data ParseError = UnexpectedToken Char
                | EOF
    deriving Show


parse :: forall a. Show a
      => Parser a
      -> String
      -> a
parse p s = report $ (evalStateT (runParser p) def{ queue = s })
  where
    report :: Either ParseError a -> a
    report (Right a) = a
    report (Left e)  = do
        error . unlines $
            [ "did not read entire string."
            , "    result: " ++ (show e)
            , ""
            , " remaining: " ++ s
            ]


unit :: a -> Parser a
unit a = return a


instance Alternative Parser where
    empty = throwError EOF
    (<|>) = option


option :: Parser a
       -> Parser a
       -> Parser a
option p q = do st <- get
                case runStateT (runParser p) st of
                  Left  _        -> q
                  Right (a, st') -> do { put st'; return a }


satisfy :: (Char -> Bool)
        -> Parser Char
satisfy f = do c <- item
               bool (throwError $ UnexpectedToken c) (unit c) $ f c


item :: Parser Char
item = do st <- get
          cs (queue st)
  where cs []      = throwError EOF
        cs (c:cs') = do st <- get
                        put st{ queue = cs' }
                        putPos c
                        return c
        putPos :: Char -> Parser Char
        putPos c = do st <- get
                      if c == '\n'
                          then put st{ col = 0, line = (line st) + 1 }
                          else put st{ col = (col st) + 1 }
                      return c


char :: Char
     -> Parser Char
char c = satisfy (c ==)


string :: String
       -> Parser String
string []     = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)


digit :: Parser Char
digit = satisfy isDigit


digit' :: Parser Int
digit' = flip (-) (ord '0') . ord <$> digit


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
