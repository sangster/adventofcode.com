{-# LANGUAGE QuasiQuotes                      #-}

module Y2018.Day04 (parts, part1, part2) where

type GuardId = Int
type Minute  = Int

data Action = Begin
            | Sleep
            | Awake
  deriving Show


data Event = Event GuardId Action Minute
  deriving Show

type OrdEvents = [Event]


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "143415")
        , (part2, Just "49944")
        ]


part1 input = ""
part2 input = ""

-- part1 :: String
--       -> String
-- part1 input = show $ sleepiestGuard * (sleepiestMinute sleepiestGuardEvents)
--   where
--     events = guardEvents . parseEvents $ lines input
--     sleepiestGuardEvents = events IM.! sleepiestGuard
--     sleepiestGuard = fst $ IM.foldrWithKey f (-1, 0) events where
--         f id es b@(_, sum) = bool b (id, sleepSum es) $ sleepSum es >= sum


-- part2 :: String
--       -> String
-- part2 input = show $ min * gId
--   where
--     events = guardEvents . parseEvents $ lines input
--     (gId, (min, count)) = head $ sortBy (\(_,(_,a)) (_,(_,b)) -> compare b a) $ IM.assocs counterMap
--     counterMap = IM.map mostIn events
--     mostIn es = largest . IM.assocs . sleepCounter $ es
--     largest pairs = head $ sortBy (\(_,a) (_,b) -> compare b a) pairs


-- parse :: GuardId -- ^ the ID from the previous line
--       -> String
--       -> Event
-- parse prevId line = Event guardId (fromKey key) (read min)
--   where
--     [_, min, key, gId] = concat (line =~ patt :: [[String]])
--     patt = [re|^\S+ \d+:(\d+)..([Gfw])\D+(\d+)?|]
--     guardId = bool prevId (read gId) (length gId > 0)
--     fromKey k
--       | k == "G"  = Begin
--       | k == "f"  = Sleep
--       | otherwise = Awake


-- parseEvents :: [String]
--             -> OrdEvents
-- parseEvents ss =
--     snd $ mapAccumL accum (-1) (sort ss)
--   where
--     accum lastId line = (id, event)
--       where event@(Event id _ _) = parse lastId line



-- guardEvents :: OrdEvents
--             -> IM.IntMap [Event]
-- guardEvents es = IM.fromListWith (\a b -> b ++ a) entries where
--     entries = map (\e@(Event id _ _) -> (id, [e])) es


-- -- | The Total number of minutes this guard was asleep.
-- sleepSum :: OrdEvents
--          -> Int
-- sleepSum es = snd $ foldl accum (0, 0) es
--   where
--     accum (startAt, sum) (Event _ Sleep min) = (min, sum)
--     accum (startAt, sum) (Event _ Awake min) = (0, sum + (min - startAt))
--     accum x _ = x


-- sleepiestMinute :: OrdEvents
--                 -> Minute
-- sleepiestMinute es = fst $ foldr1 largest (IM.assocs $ sleepCounter es)
--   where
--     largest x@(_, xc) y@(_, yc) = bool x y (yc > xc)


-- -- | A map of clock-minutes (0-59) to the number of times the guard is asleep in
-- -- that minute in the given events.
-- sleepCounter :: OrdEvents
--              -> IM.IntMap Int
-- sleepCounter es = snd $ foldl accum (0, initialMap) es
--   where
--     initialMap = IM.fromAscList $ map (flip (,) 0) [0..59]
--     accum (startAt, map') (Event _ Sleep min) = (min, map')
--     -- accum (startAt, map') (Event _ Awake min) = (0,
--     --   where
--     --     map'' = foldr inc map' [startAt..(min - 1)]
--     --     inc min' m = IM.adjust (+1) min' m
--     -- accum x _ = x
