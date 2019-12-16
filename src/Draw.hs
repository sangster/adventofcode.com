module Draw
    ( block
    , space
    , target
    , lightShade
    , mediumShade
    , darkShade
    , origin
    , avatar
    , dot
    , showHashMapP
    , showHashMap
    ) where

import qualified Data.HashMap.Strict as M
import           Data.Bool

block   = [blockCh]
blockCh = '█'

space   = [spaceCh]
spaceCh = ' '

dot   = [dotCh]
dotCh = '·'

target   = [targetCh]
targetCh = '⦿'

lightShade   = [lightShadeCh]
lightShadeCh = '░'

mediumShade   = [mediumShadeCh]
mediumShadeCh = '▒'

darkShade   = [darkShadeCh]
darkShadeCh = '▓'

origin   = [originCh]
originCh = '◯'

avatar   = [avatarCh]
avatarCh = '☻'


showHashMapP :: ((Int,Int) -> Maybe a -> String)
             -> M.HashMap (Int,Int) a
             -> String
showHashMapP p hMap = tail $ grid >>= print
  where
    print (x,y) = bool str ('\n':str) $ x == minX
      where str = p (x,y) $ M.lookup (x,y) hMap

    grid = [(x,y) | y <- [minY .. maxY], x <- [minX .. maxX]]
    tileMap = M.fromList $ map mapper (M.keys hMap)
    mapper (x,y) = ((x-minX, y-minY), ((M.!) hMap (x,y)))

    (minX,minY,maxX,maxY) = foldr boundsFold (0,0,0,0) (M.keys hMap)
    boundsFold (x,y) (nX,nY,xX,xY) = (minimum [x,nX], minimum [y,nY],
                                      maximum [x,xX], maximum [y,xY])


showHashMap :: (Show a)
            => a
            -> M.HashMap (Int,Int) a
            -> String
showHashMap d = showHashMapP $ \_ -> maybe (show d) show
