module Y2020.Day14 (parts) where

import           Data.Bits
import           Data.Bool (bool)
import qualified Data.HashMap.Strict as M
import           Data.List (foldl')
import           Data.Word (Word64)
import           Parser


parts = ( (part1, Just "4886706177792")
        , (part2, Just "3348493585827")
        , parse $ splitSome whitespace (setMask <|> setMem)
        )


part1 :: Program -> String
part1 prog = show $ M.foldr (+) 0 result
  where
    (RAM _ result) = foldl' exec (RAM (Mask 0 0) M.empty) prog

    exec (RAM _    mem) (SetMask mask'  ) = (RAM mask' mem )
    exec (RAM mask mem) (SetMem addr val) = (RAM mask  mem')
      where
        mem' = M.insert addr val' mem
        val' = applyMask mask val


part2 :: Program -> String
part2 prog = show $ M.foldr (+) 0 result
  where
    (RAM _ result) = foldl' exec (RAM (Mask 0 0) M.empty) prog

    exec (RAM _    mem) (SetMask mask'  ) = (RAM mask' mem )
    exec (RAM mask mem) (SetMem addr val) = (RAM mask  mem')
      where
        mem' = foldr setFloating mem (floatingMasks mask)
        setFloating mask' mem'' = M.insert (applyMask mask' addr) val mem''


type Word36 = Word64
type Addr   = Word36
data Mask = Mask { onMask :: Word36
                 , offMask :: Word36
                 } deriving Show

type Program = [Op]
data Op = SetMask Mask
        | SetMem Addr Word36
        deriving Show

data RAM = RAM Mask (M.HashMap Addr Word36)
           deriving Show


applyMask (Mask on off) val = (val .|. on) .&. off


floatingMasks (Mask on off) = mkFloatingMask <$> floatingPiars
  where
    floatingPiars = foldr addCombos [(on, off)] [0..35]
    addCombos i masks
      | isFloating = foldr (splitMasks i) [] masks
      | otherwise  = masks
      where
        isFloating = not (testBit on i) && testBit off i

    splitMasks i (on, off) masks =
        (setBit on i, off):(on, clearBit off i):masks

    mkFloatingMask (on, off) = Mask on (off .|. zeroMask)
    zeroMask = foldr (\i n -> bool n (setBit n i) $ isZero i) 0 [0..35]
      where
        isZero i = not $ testBit on i || testBit off i


setMask :: Parser Op
setMask = do string "mask = "
             mayDigits <- some maskDigit
             pure . SetMask . uncurry Mask $ splitMasks mayDigits

  where
    maskDigit = (symbol (Just  0) $ char '0')
            <|> (symbol (Just  1) $ char '1')
            <|> (symbol (Nothing) $ char 'X')

    splitMasks = foldr shiftMask (0,0) . reverse
    shiftMask (Just  1) (on, off) = (shiftL on 1 .|. 1, shiftL off 1 .|. 1)
    shiftMask (Just  0) (on, off) = (shiftL on 1      , shiftL off 1      )
    shiftMask (Nothing) (on, off) = (shiftL on 1      , shiftL off 1 .|. 1)


setMem :: Parser Op
setMem = do string "mem["
            addr <- natural
            string "] = "
            value <- natural
            pure $ SetMem addr value
