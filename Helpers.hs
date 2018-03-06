module Helpers
( for
, showDay
, safeHead
, thd, snd3, fst3
, language
, showTime
, today
)
where

import Prelude
import Data.Time
import Data.List(find)
import Data.Text(Text)
import qualified Data.Text

today :: IO Day
today = getCurrentTime
  >>= return . utctDay

thd :: (t1,t2,t3) -> t3
thd (_, _, x) = x

fst3 :: (t1,t2,t3) -> t1
fst3 (x, _, _) = x

snd3 :: (t1,t2,t3) -> t2
snd3 (_, x, _) = x

for :: [a] -> (a -> b) -> [b]
for = flip map

showDay :: Day -> String
showDay date =
  (show d) ++ "." ++ (show m) ++ "." ++ (show y)
  where
    (y,m,d) = toGregorian date

showTime :: TimeZone -> UTCTime -> String
showTime tz time =
  let
    local = utcToLocalTime tz time
    day = showDay $ localDay local
  in
    day ++ " " ++ formatTime defaultTimeLocale "%H:%M" local

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- choose first 2 letter language
language :: [Text] -> String
language langs = maybe "en" Data.Text.unpack $
  find (\l -> (Data.Text.length l) == 2) langs