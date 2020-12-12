module Util.Color
    ( ansiForeground
    , ansiBackground
    , module System.Console.ANSI
    ) where

import System.Console.ANSI


ansiForeground :: ColorIntensity
               -> Color
               -> String
               -> String
ansiForeground ci c str =
    concat [ setSGRCode [SetColor Foreground ci c]
           , str
           , setSGRCode [Reset]
           ]


ansiBackground :: ColorIntensity
               -> Color
               -> String
               -> String
ansiBackground ci c str =
    concat [ setSGRCode [SetColor Background ci c]
           , str
           , setSGRCode [Reset]
           ]
