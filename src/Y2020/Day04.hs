module Y2020.Day04 (parts) where

import Data.Maybe (isJust)
import Parser

parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "256")
        , (part2, Just "198")
        ]


part1 input = show . length $ filter valid passports
  where
    passports = parse (some passport) input
    valid (Passport entries) = all (hasField entries)
                        $ filter required [minBound..]


part2 input = show . length $ filter valid passports
  where
    passports = parse (some passport) input
    valid pass = and $ [validFields, validEntries] <*> pure pass
    validFields (Passport entries) = all (hasField entries)
                                   $ filter required [minBound..]
    validEntries (Passport entries) = all validEntry entries


data LengthUnit = Metric | American
data Passport = Passport [Entry] deriving Show
type Entry = (Field, String)
data Field = BirthYear
           | IssueYear
           | ExpirationYear
           | Height
           | HairColor
           | EyeColor
           | PassportId
           | CountryId
  deriving (Bounded, Enum, Eq, Show)


required :: Field -> Bool
required CountryId = False
required _         = True


hasField :: [Entry] -> Field -> Bool
hasField fs f = isJust $ lookup f fs


passport :: Parser Passport
passport =  do pass <- Passport <$> some entry
               char '\n' <|> pure '\n'
               pure pass


entry :: Parser Entry
entry = do field' <- field
           char ':'
           word' <- word
           oneOf " \n" <|> pure ' '
           pure (field', word')


field :: Parser Field
field = (symbol BirthYear      $ string "byr")
    <|> (symbol IssueYear      $ string "iyr")
    <|> (symbol ExpirationYear $ string "eyr")
    <|> (symbol Height         $ string "hgt")
    <|> (symbol HairColor      $ string "hcl")
    <|> (symbol EyeColor       $ string "ecl")
    <|> (symbol PassportId     $ string "pid")
    <|> (symbol CountryId      $ string "cid")


validEntry :: Entry -> Bool
validEntry (field, word) = validate field
  where
    check p f = either (const False) f $ evalParser p word

    validate BirthYear = check number (\y -> y >= 1920 && y <= 2002)
    validate IssueYear = check number (\y -> y >= 2010 && y <= 2020)
    validate ExpirationYear = check number (\y -> y >= 2020 && y <= 2030)
    validate Height = check height validHeight
      where
        validHeight (Metric, h)   = h >= 150 && h <= 193
        validHeight (American, h) = h >=  59 && h <=  76
        height = do h <- natural
                    lenUnit <- (symbol Metric   $ string "cm")
                           <|> (symbol American $ string "in")
                    pure (lenUnit, h)
    validate HairColor = check color ((== 6) . length)
      where
        color = char '#' >> some (oneOf $ ['0'..'9'] ++ ['a'..'f'])
    validate EyeColor = check color (const True)
      where
        color = string "amb" <|> string "blu" <|> string "brn"
            <|> string "gry" <|> string "grn" <|> string "hzl"
            <|> string "oth"
    validate PassportId = check (many digit) ((== 9) . length)
    validate _ = check (pure ()) (const True)
