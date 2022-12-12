-- This File is generated by scripts/generate-sources.sh.
{-# LANGUAGE ImportQualifiedPost, TemplateHaskell #-}
module GeneratedSolutions (inputsDir, lookupInput, solutions, Year, Day) where

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Solution
import Y2018.Day01 qualified
import Y2018.Day02 qualified
import Y2018.Day03 qualified
import Y2018.Day04 qualified
import Y2018.Day05 qualified
import Y2019.Day01 qualified
import Y2019.Day02 qualified
import Y2019.Day03 qualified
import Y2019.Day04 qualified
import Y2019.Day05 qualified
import Y2019.Day06 qualified
import Y2019.Day07 qualified
import Y2019.Day08 qualified
import Y2019.Day09 qualified
import Y2019.Day10 qualified
import Y2019.Day11 qualified
import Y2019.Day12 qualified
import Y2019.Day13 qualified
import Y2019.Day14 qualified
import Y2019.Day15 qualified
import Y2019.Day16 qualified
import Y2019.Day17 qualified
import Y2019.Day18 qualified
import Y2019.Day19 qualified
import Y2019.Day20 qualified
import Y2019.Day21 qualified
import Y2019.Day22 qualified
import Y2019.Day23 qualified
import Y2019.Day24 qualified
import Y2019.Day25 qualified
import Y2020.Day01 qualified
import Y2020.Day02 qualified
import Y2020.Day03 qualified
import Y2020.Day04 qualified
import Y2020.Day05 qualified
import Y2020.Day06 qualified
import Y2020.Day07 qualified
import Y2020.Day08 qualified
import Y2020.Day09 qualified
import Y2020.Day10 qualified
import Y2020.Day11 qualified
import Y2020.Day12 qualified
import Y2020.Day13 qualified
import Y2020.Day14 qualified
import Y2020.Day15 qualified
import Y2020.Day16 qualified
import Y2020.Day17 qualified
import Y2020.Day18 qualified
import Y2020.Day19 qualified
import Y2020.Day20 qualified
import Y2020.Day21 qualified
import Y2020.Day22 qualified
import Y2020.Day23 qualified
import Y2020.Day24 qualified
import Y2020.Day25 qualified
import Y2021.Day01 qualified
import Y2021.Day02 qualified
import Y2021.Day03 qualified
import Y2021.Day04 qualified
import Y2021.Day05 qualified
import Y2021.Day06 qualified
import Y2021.Day07 qualified
import Y2021.Day08 qualified
import Y2021.Day09 qualified
import Y2021.Day10 qualified
import Y2021.Day11 qualified
import Y2021.Day12 qualified
import Y2021.Day13 qualified
import Y2021.Day14 qualified
import Y2021.Day15 qualified
import Y2021.Day16 qualified
import Y2021.Day17 qualified
import Y2021.Day18 qualified
import Y2021.Day19 qualified
import Y2021.Day20 qualified
import Y2021.Day21 qualified
import Y2021.Day22 qualified
import Y2021.Day23 qualified
import Y2021.Day24 qualified
import Y2021.Day25 qualified
import Y2022.Day01 qualified
import Y2022.Day02 qualified
import Y2022.Day03 qualified
import Y2022.Day04 qualified
import Y2022.Day05 qualified
import Y2022.Day06 qualified
import Y2022.Day07 qualified
import Y2022.Day08 qualified

type Year = String
type Day  = String

solutions :: [((Year, Day), Solveable DaySolution)]
solutions = concat
    [ daysFor "2018" days2018
    , daysFor "2019" days2019
    , daysFor "2020" days2020
    , daysFor "2021" days2021
    , daysFor "2022" days2022
    ]
  where
    days2018 =
      [ solve Y2018.Day01.parts
      , solve Y2018.Day02.parts
      , solve Y2018.Day03.parts
      , solve Y2018.Day04.parts
      , solve Y2018.Day05.parts
      ]
    days2019 =
      [ solve Y2019.Day01.parts
      , solve Y2019.Day02.parts
      , solve Y2019.Day03.parts
      , solve Y2019.Day04.parts
      , solve Y2019.Day05.parts
      , solve Y2019.Day06.parts
      , solve Y2019.Day07.parts
      , solve Y2019.Day08.parts
      , solve Y2019.Day09.parts
      , solve Y2019.Day10.parts
      , solve Y2019.Day11.parts
      , solve Y2019.Day12.parts
      , solve Y2019.Day13.parts
      , solve Y2019.Day14.parts
      , solve Y2019.Day15.parts
      , solve Y2019.Day16.parts
      , solve Y2019.Day17.parts
      , solve Y2019.Day18.parts
      , solve Y2019.Day19.parts
      , solve Y2019.Day20.parts
      , solve Y2019.Day21.parts
      , solve Y2019.Day22.parts
      , solve Y2019.Day23.parts
      , solve Y2019.Day24.parts
      , solve Y2019.Day25.parts
      ]
    days2020 =
      [ solve Y2020.Day01.parts
      , solve Y2020.Day02.parts
      , solve Y2020.Day03.parts
      , solve Y2020.Day04.parts
      , solve Y2020.Day05.parts
      , solve Y2020.Day06.parts
      , solve Y2020.Day07.parts
      , solve Y2020.Day08.parts
      , solve Y2020.Day09.parts
      , solve Y2020.Day10.parts
      , solve Y2020.Day11.parts
      , solve Y2020.Day12.parts
      , solve Y2020.Day13.parts
      , solve Y2020.Day14.parts
      , solve Y2020.Day15.parts
      , solve Y2020.Day16.parts
      , solve Y2020.Day17.parts
      , solve Y2020.Day18.parts
      , solve Y2020.Day19.parts
      , solve Y2020.Day20.parts
      , solve Y2020.Day21.parts
      , solve Y2020.Day22.parts
      , solve Y2020.Day23.parts
      , solve Y2020.Day24.parts
      , solve Y2020.Day25.parts
      ]
    days2021 =
      [ solve Y2021.Day01.parts
      , solve Y2021.Day02.parts
      , solve Y2021.Day03.parts
      , solve Y2021.Day04.parts
      , solve Y2021.Day05.parts
      , solve Y2021.Day06.parts
      , solve Y2021.Day07.parts
      , solve Y2021.Day08.parts
      , solve Y2021.Day09.parts
      , solve Y2021.Day10.parts
      , solve Y2021.Day11.parts
      , solve Y2021.Day12.parts
      , solve Y2021.Day13.parts
      , solve Y2021.Day14.parts
      , solve Y2021.Day15.parts
      , solve Y2021.Day16.parts
      , solve Y2021.Day17.parts
      , solve Y2021.Day18.parts
      , solve Y2021.Day19.parts
      , solve Y2021.Day20.parts
      , solve Y2021.Day21.parts
      , solve Y2021.Day22.parts
      , solve Y2021.Day23.parts
      , solve Y2021.Day24.parts
      , solve Y2021.Day25.parts
      ]
    days2022 =
      [ solve Y2022.Day01.parts
      , solve Y2022.Day02.parts
      , solve Y2022.Day03.parts
      , solve Y2022.Day04.parts
      , solve Y2022.Day05.parts
      , solve Y2022.Day06.parts
      , solve Y2022.Day07.parts
      , solve Y2022.Day08.parts
      ]

daysFor :: Year
        -> [Solveable DaySolution]
        -> [((Year, Day), Solveable DaySolution)]
daysFor y ps = [ ((y, fmt d), p) | (d, p) <- zip [1..] ps]
  where
    fmt d | d < 10    = '0':show d
          | otherwise = show d

lookupInput :: FilePath -> Maybe String
lookupInput fp = toString <$> lookup fp inputsDir

inputsDir :: [(FilePath, BS.ByteString)]
inputsDir =
    [ ("2018/01", $(makeRelativeToProject "inputs/2018/01" >>= embedFile))
    , ("2018/02", $(makeRelativeToProject "inputs/2018/02" >>= embedFile))
    , ("2018/03", $(makeRelativeToProject "inputs/2018/03" >>= embedFile))
    , ("2018/04", $(makeRelativeToProject "inputs/2018/04" >>= embedFile))
    , ("2018/05", $(makeRelativeToProject "inputs/2018/05" >>= embedFile))
    , ("2019/01", $(makeRelativeToProject "inputs/2019/01" >>= embedFile))
    , ("2019/02", $(makeRelativeToProject "inputs/2019/02" >>= embedFile))
    , ("2019/03", $(makeRelativeToProject "inputs/2019/03" >>= embedFile))
    , ("2019/04", $(makeRelativeToProject "inputs/2019/04" >>= embedFile))
    , ("2019/05", $(makeRelativeToProject "inputs/2019/05" >>= embedFile))
    , ("2019/06", $(makeRelativeToProject "inputs/2019/06" >>= embedFile))
    , ("2019/07", $(makeRelativeToProject "inputs/2019/07" >>= embedFile))
    , ("2019/08", $(makeRelativeToProject "inputs/2019/08" >>= embedFile))
    , ("2019/09", $(makeRelativeToProject "inputs/2019/09" >>= embedFile))
    , ("2019/10", $(makeRelativeToProject "inputs/2019/10" >>= embedFile))
    , ("2019/11", $(makeRelativeToProject "inputs/2019/11" >>= embedFile))
    , ("2019/12", $(makeRelativeToProject "inputs/2019/12" >>= embedFile))
    , ("2019/13", $(makeRelativeToProject "inputs/2019/13" >>= embedFile))
    , ("2019/14", $(makeRelativeToProject "inputs/2019/14" >>= embedFile))
    , ("2019/15", $(makeRelativeToProject "inputs/2019/15" >>= embedFile))
    , ("2019/16", $(makeRelativeToProject "inputs/2019/16" >>= embedFile))
    , ("2019/17", $(makeRelativeToProject "inputs/2019/17" >>= embedFile))
    , ("2019/18", $(makeRelativeToProject "inputs/2019/18" >>= embedFile))
    , ("2019/19", $(makeRelativeToProject "inputs/2019/19" >>= embedFile))
    , ("2019/20", $(makeRelativeToProject "inputs/2019/20" >>= embedFile))
    , ("2019/21", $(makeRelativeToProject "inputs/2019/21" >>= embedFile))
    , ("2019/22", $(makeRelativeToProject "inputs/2019/22" >>= embedFile))
    , ("2019/23", $(makeRelativeToProject "inputs/2019/23" >>= embedFile))
    , ("2019/24", $(makeRelativeToProject "inputs/2019/24" >>= embedFile))
    , ("2019/25", $(makeRelativeToProject "inputs/2019/25" >>= embedFile))
    , ("2020/01", $(makeRelativeToProject "inputs/2020/01" >>= embedFile))
    , ("2020/02", $(makeRelativeToProject "inputs/2020/02" >>= embedFile))
    , ("2020/03", $(makeRelativeToProject "inputs/2020/03" >>= embedFile))
    , ("2020/04", $(makeRelativeToProject "inputs/2020/04" >>= embedFile))
    , ("2020/05", $(makeRelativeToProject "inputs/2020/05" >>= embedFile))
    , ("2020/06", $(makeRelativeToProject "inputs/2020/06" >>= embedFile))
    , ("2020/07", $(makeRelativeToProject "inputs/2020/07" >>= embedFile))
    , ("2020/08", $(makeRelativeToProject "inputs/2020/08" >>= embedFile))
    , ("2020/09", $(makeRelativeToProject "inputs/2020/09" >>= embedFile))
    , ("2020/10", $(makeRelativeToProject "inputs/2020/10" >>= embedFile))
    , ("2020/11", $(makeRelativeToProject "inputs/2020/11" >>= embedFile))
    , ("2020/12", $(makeRelativeToProject "inputs/2020/12" >>= embedFile))
    , ("2020/13", $(makeRelativeToProject "inputs/2020/13" >>= embedFile))
    , ("2020/14", $(makeRelativeToProject "inputs/2020/14" >>= embedFile))
    , ("2020/15", $(makeRelativeToProject "inputs/2020/15" >>= embedFile))
    , ("2020/16", $(makeRelativeToProject "inputs/2020/16" >>= embedFile))
    , ("2020/17", $(makeRelativeToProject "inputs/2020/17" >>= embedFile))
    , ("2020/18", $(makeRelativeToProject "inputs/2020/18" >>= embedFile))
    , ("2020/19", $(makeRelativeToProject "inputs/2020/19" >>= embedFile))
    , ("2020/20", $(makeRelativeToProject "inputs/2020/20" >>= embedFile))
    , ("2020/21", $(makeRelativeToProject "inputs/2020/21" >>= embedFile))
    , ("2020/22", $(makeRelativeToProject "inputs/2020/22" >>= embedFile))
    , ("2020/23", $(makeRelativeToProject "inputs/2020/23" >>= embedFile))
    , ("2020/24", $(makeRelativeToProject "inputs/2020/24" >>= embedFile))
    , ("2020/25", $(makeRelativeToProject "inputs/2020/25" >>= embedFile))
    , ("2021/01", $(makeRelativeToProject "inputs/2021/01" >>= embedFile))
    , ("2021/02", $(makeRelativeToProject "inputs/2021/02" >>= embedFile))
    , ("2021/03", $(makeRelativeToProject "inputs/2021/03" >>= embedFile))
    , ("2021/04", $(makeRelativeToProject "inputs/2021/04" >>= embedFile))
    , ("2021/05", $(makeRelativeToProject "inputs/2021/05" >>= embedFile))
    , ("2021/06", $(makeRelativeToProject "inputs/2021/06" >>= embedFile))
    , ("2021/07", $(makeRelativeToProject "inputs/2021/07" >>= embedFile))
    , ("2021/08", $(makeRelativeToProject "inputs/2021/08" >>= embedFile))
    , ("2021/09", $(makeRelativeToProject "inputs/2021/09" >>= embedFile))
    , ("2021/10", $(makeRelativeToProject "inputs/2021/10" >>= embedFile))
    , ("2021/11", $(makeRelativeToProject "inputs/2021/11" >>= embedFile))
    , ("2021/12", $(makeRelativeToProject "inputs/2021/12" >>= embedFile))
    , ("2021/13", $(makeRelativeToProject "inputs/2021/13" >>= embedFile))
    , ("2021/14", $(makeRelativeToProject "inputs/2021/14" >>= embedFile))
    , ("2021/15", $(makeRelativeToProject "inputs/2021/15" >>= embedFile))
    , ("2021/16", $(makeRelativeToProject "inputs/2021/16" >>= embedFile))
    , ("2021/17", $(makeRelativeToProject "inputs/2021/17" >>= embedFile))
    , ("2021/18", $(makeRelativeToProject "inputs/2021/18" >>= embedFile))
    , ("2021/19", $(makeRelativeToProject "inputs/2021/19" >>= embedFile))
    , ("2021/20", $(makeRelativeToProject "inputs/2021/20" >>= embedFile))
    , ("2021/21", $(makeRelativeToProject "inputs/2021/21" >>= embedFile))
    , ("2021/22", $(makeRelativeToProject "inputs/2021/22" >>= embedFile))
    , ("2021/23", $(makeRelativeToProject "inputs/2021/23" >>= embedFile))
    , ("2021/24", $(makeRelativeToProject "inputs/2021/24" >>= embedFile))
    , ("2021/25", $(makeRelativeToProject "inputs/2021/25" >>= embedFile))
    , ("2022/01", $(makeRelativeToProject "inputs/2022/01" >>= embedFile))
    , ("2022/02", $(makeRelativeToProject "inputs/2022/02" >>= embedFile))
    , ("2022/03", $(makeRelativeToProject "inputs/2022/03" >>= embedFile))
    , ("2022/04", $(makeRelativeToProject "inputs/2022/04" >>= embedFile))
    , ("2022/05", $(makeRelativeToProject "inputs/2022/05" >>= embedFile))
    , ("2022/06", $(makeRelativeToProject "inputs/2022/06" >>= embedFile))
    , ("2022/07", $(makeRelativeToProject "inputs/2022/07" >>= embedFile))
    , ("2022/08", $(makeRelativeToProject "inputs/2022/08" >>= embedFile))
    ]
