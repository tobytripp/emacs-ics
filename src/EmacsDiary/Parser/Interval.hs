-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.Interval where

import EmacsDiary.Parser.Tokens

import Text.Parsec (sepBy1, unexpected)
import Text.Parsec.String (Parser)
import Data.Time.Clock (
  addUTCTime,
  NominalDiffTime,
  utctDayTime,
  utctDay,
  UTCTime)
import Data.Time.Format (iso8601DateFormat, formatTime, parseTimeM, defaultTimeLocale)
import Text.Printf (printf)

dateFormat = "%e %b %Y"
timeFormat = "%H:%M"
showTimeFormat = "%H:%M:%S%z"
locale = defaultTimeLocale

class AsUTC a where
      asUTC :: a -> Either String UTCTime

data Date = Date { dateDay   :: Integer,
                   dateMonth :: Integer,
                   dateYear  :: Integer
                 } deriving (Eq)
instance Show Date where
         show Date {dateDay=d, dateMonth=m, dateYear=y} =
            printf "%d-%d-%d" y m d
instance AsUTC Date where
         asUTC d = case parsed of
                     (Just x) -> Right x
                     Nothing -> Left $ "`parseTime` unable to parse “" ++ (show d) ++ "”"
             where
               parsed = parseTime . show $ d
               parseTime = parseTimeM True locale dateFormat

data Time = Time { timeHour   :: Integer
                 , timeMinute :: Integer
                 } deriving (Eq)
instance Show Time where
         show t = iso8601DateFormat (Just showTimeFormat)
instance AsUTC Time where
         asUTC t = case parsed of
                     (Just x) -> Right x
                     Nothing -> Left $ "Unable to parse " ++ (show t)
           where
             parsed = parseTime . show $ t
             parseTime = parseTimeM True locale timeFormat

instance Num Time where
  a + b  = fromInteger $ (seconds a) + (seconds b)
  a * b  = fromInteger $ (seconds a) * (seconds b)
  abs    = fromInteger . abs . seconds
  signum = fromInteger . signum . seconds
  negate = fromInteger . negate . seconds
  fromInteger a = Time h m
    where
      i = fromInteger a
      h = quot i 3600
      m = div s 60
      s = rem i 3600

timeFromList :: [Integer] -> Time
timeFromList (h:m:_) = Time h m

decided :: Parser (Either String a) -> Parser a
decided p = do
        e <- p
        case e of
          (Left s)  -> unexpected s
          (Right r) -> return r

seconds :: Time -> Integer
seconds (Time h m)= 3600*h + 60*m

timeToNDiff :: Time -> NominalDiffTime
timeToNDiff (Time h m) = fromInteger (3600*h + 60*m)

addTime :: UTCTime -> Time -> UTCTime
addTime a t = addUTCTime (timeToNDiff t) a

day   = numeric
year  = numeric

timeS :: Parser Time
timeS = timeFromList <$> sepBy1 numeric (symbol ":")

dateS :: Parser Date
dateS = Date <$> day <*> month <*> year

time :: UTCTime -> Parser UTCTime
time datetime = addTime datetime <$> timeS

date :: Parser UTCTime
date = decided p
  where
    p = asUTC <$> dateS

month :: Parser Integer
month = monthOrdinal
