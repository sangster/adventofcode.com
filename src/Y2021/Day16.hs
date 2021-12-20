{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Y2021.Day16 (parts) where

import Control.Monad              (liftM, mzero, replicateM)
import Control.Monad.State.Strict (MonadState(..), State(..), evalState, state)
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Data.Bits                  (testBit)
import Data.Bool                  (bool)
import Data.Char                  (ord)


parts = ( (part1, Just "953")
        , (part2, Just "246225449979")
        , (\s -> evalBITS "<Nothing>" id (s, []))
        )


part1 :: (BITS String -> String) -> String
part1 f = f $ liftM (show . sumVersions . (: [])) packet
  where
    sumVersions [] = 0
    sumVersions (p:ps) = sum' p + sumVersions ps
      where
        sum' (Literal v _) = v
        sum' op = ver op + sumVersions (subpacks op)


part2 :: (BITS String -> String) -> String
part2 f = f $ liftM (show . value) packet


type Version = Int
type TypeId  = Int
type Binary  = [Bool]

data Packet = Literal     { ver :: Version, val :: Int }
            | Sum         { ver :: Version, subpacks :: [Packet] }
            | Product     { ver :: Version, subpacks :: [Packet] }
            | Minimum     { ver :: Version, subpacks :: [Packet] }
            | Maximum     { ver :: Version, subpacks :: [Packet] }
            | GreaterThan { ver :: Version, subpacks :: [Packet] }
            | LessThan    { ver :: Version, subpacks :: [Packet] }
            | EqualTo     { ver :: Version, subpacks :: [Packet] }
            deriving Show


-- | A monad to return bits (ie: Bools) one at a time. The state contains the
--   list of hex chars and any remainin bits from the most recent nibble.
newtype BITS a = BITS { runBITS :: MaybeT (State (String, Binary)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (String, Binary)
    )


evalBITS :: b -> (a -> b) -> (String, Binary) -> BITS a -> b
evalBITS def f st p = case evalState (runMaybeT $ runBITS p) st of
                      Just ss -> f ss
                      Nothing -> def


nextBit :: BITS Bool
nextBit = BITS . MaybeT . state
        $ \case
            ("",   [])   -> (Nothing, ("", []))
            (s:ss, [])   -> let (b:bs) = hexCharToNibble s
                            in (Just b, (ss, bs))
            (ss, (b:bs)) -> (Just b, (ss, bs))


takeBits :: Int -> BITS Binary
takeBits = flip replicateM nextBit


-- | Parse the next N bits as an Int.
binaryInt :: Int -> BITS Int
binaryInt = liftM toInt . takeBits


-- | Parse as many Packets as possible, from the remaining bits.
packets :: BITS [Packet]
packets = do p <- packet
             cont <- hasMore
             if cont then liftM (p :) packets else pure [p]
  where
    hasMore = BITS . MaybeT . state $ \case
                                        ("", []) -> (Just False, ("", []))
                                        st       -> (Just True, st)


packet :: BITS Packet
packet = do v <- version
            typeId >>= \case 4 -> literal v
                             n -> operator v n
  where
    version = binaryInt 3
    typeId  = binaryInt 3


-- | Parse a given number of Packets.
packetsN :: Int -> BITS [Packet]
packetsN 0 = pure []
packetsN n = do p <- packet
                packetsN (n-1) >>= pure . (p:)


literal :: Version -> BITS Packet
literal v = do bits <- numBits
               pure $ Literal v (toInt bits)
  where
    numBits :: BITS Binary
    numBits = do continue <- nextBit
                 nibble   <- takeBits 4
                 bool (pure nibble) (liftM (nibble ++) numBits) continue


operator :: Version -> TypeId -> BITS Packet
operator v tid = do lid <- nextBit
                    if lid
                    then binaryInt 11 >>= packetsN >>= mkOperator
                    else binaryInt 15 >>= takeBits >>= subPacks
  where
    subPacks b = evalBITS (BITS mzero) mkOperator ("", b) packets
    mkOperator = pure . opConstructor v
    opConstructor = case tid of
                      0 -> Sum
                      1 -> Product
                      2 -> Minimum
                      3 -> Maximum
                      5 -> GreaterThan
                      6 -> LessThan
                      _ -> EqualTo


value :: Packet -> Int
value (Literal _ n)         = n
value (Sum _ ps)            = sum      $ value <$> ps
value (Product _ ps)        = product  $ value <$> ps
value (Minimum _ ps)        = minimum  $ value <$> ps
value (Maximum _ ps)        = maximum  $ value <$> ps
value (GreaterThan _ [a,b]) = bool 0 1 $ value a > value b
value (LessThan _ [a,b])    = bool 0 1 $ value a < value b
value (EqualTo _ [a,b])     = bool 0 1 $ value a == value b
value _                     = error "invalid packet"


-- | Convert a Binary into an Int (big-endian).
toInt :: Binary -> Int
toInt = fst . foldr (\b (n, pow) -> (n + bool 0 pow b, pow * 2)) (0, 1)


-- | Convert a hex Char into the 4 bits it represents, big-endian.
hexCharToNibble :: Char -> Binary
hexCharToNibble c = reverse $ testBit n <$> [0..3]
  where
    n = i - (bool (ord '0') (ord 'A' - 10) (i >= ord 'A'))
    i = ord c
