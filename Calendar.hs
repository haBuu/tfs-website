module Calendar
( calendarWidget
)
where

import Import hiding (for)

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.MonthDay

import Helpers
import Model.Event

data Calendar = Calendar
  { months :: [Month]
  }
  deriving (Show)

data Month = Month
  { month :: Int
  , year :: Integer
  , weeks :: [[Day]]
  }
  deriving (Show)

calendarWidget :: Handler Widget
calendarWidget = do
  day <- liftIO today
  -- TODO: calculate exact range
  -- actually it would be better if the range was given as param
  -- to calendar function but for now range of two years will have all the events
  -- we might need
  let start = addDays (-365) day
      end = addDays 365 day
  events <- runDB $ getEvents start end
  return $ calendarWidget' day events

calendarWidget' :: Day -> [Entity Event] -> Widget
calendarWidget' currentDay events = [whamlet|
  <div #calendar-container>
    $forall m <- months $ calendar currentDay
      <table #calendar :(not $ sameMonth (month m) currentDay):style="display: none;">
        <thead>
          <tr>
            <th #prev .arrow><
            <th colspan=5>_{monthMsg $ month m} #{show $ year m}
            <th #next .arrow>>
          <tr>
            <th>_{MsgMondayShort}
            <th>_{MsgTuesdayShort}
            <th>_{MsgWednesdayShort}
            <th>_{MsgThursdayShort}
            <th>_{MsgFridayShort}
            <th>_{MsgSaturdayShort}
            <th>_{MsgSundayShort}
        <tbody>
          $forall week <- weeks m
            <tr>
              $forall day <- week
                <td title=#{eventTitles events day} :(not $ sameMonth (month m) day):.text-muted :(not $ null $ eventTitles events day):.event :(currentDay == day):.current-day>
                  $if not $ null $ eventTitles events day
                    <a href=@{DayR day}>
                      #{show $ thd $ toGregorian day}
                  $else
                    #{show $ thd $ toGregorian day}
|]

eventTitles :: [Entity Event] -> Day -> Text
eventTitles allEvents day = intercalate ", " $ map eventTitle events
  where
    events = filter (\e -> day `elem` eventDays e) $ map entityVal allEvents

eventDays :: Event -> [Day]
eventDays event =
  case eventEndDate event of
    Just end -> [addDays i start | i <- [0..diffDays end start]]
    Nothing -> [start]
  where
    start = eventStartDate event

sameMonth :: Int -> Day -> Bool
sameMonth month day = month == (snd3 $ toGregorian day)

monthMsg :: Int -> AppMessage
monthMsg 1 = MsgJanuary
monthMsg 2 = MsgFebruary
monthMsg 3 = MsgMarch
monthMsg 4 = MsgApril
monthMsg 5 = MsgMay
monthMsg 6 = MsgJune
monthMsg 7 = MsgJuly
monthMsg 8 = MsgAugust
monthMsg 9 = MsgSeptember
monthMsg 10 = MsgOctober
monthMsg 11 = MsgNovember
monthMsg 12 = MsgDevember
monthMsg _ = MsgUnknownMonth

-- 12 months: current month, 5 months in the past and 6 months to the future
calendar :: Day -> Calendar
calendar day =
  let
    (year, month, _) = toGregorian day
  in
    Calendar $ for (months year month) $ \(y, m) -> Month m y $ oneMonth y m
  where
    months :: Integer -> Int -> [(Integer, Int)]
    months year month = for [0..11] $ \i -> validYearAndMonth year (month - 5 + i)
    -- this only works for a range of one year
    validYearAndMonth :: Integer -> Int -> (Integer, Int)
    validYearAndMonth y m =
      if m < 1
        then (y - 1, 12 + m) -- previous year
        else if m > 12
          then (y + 1, m - 12) -- next year
          else (y, m) -- all is good

oneMonth :: Integer -> Int -> [[Day]]
oneMonth year month = toWeeks $ allDaysInMonth year month

-- break 42 days in to 6 weeks
toWeeks :: [Day] -> [[Day]]
toWeeks (d1:d2:d3:d4:d5:d6:d7:days) = [d1,d2,d3,d4,d5,d6,d7]:toWeeks days
toWeeks _ = []

-- return a list of 42 days that has all the days in given month
-- and some additional days from previous and next month
allDaysInMonth :: Integer -> Int -> [Day]
allDaysInMonth year month =
  let
    -- first day of this month (week date)
    firstDay = dayOfWeek $ fromGregorian year month 1
    (prevYear, prevMonth) = prevMonthAndYear year month
    (nextYear, nextMonth) = nextMonthAndYear year month
    -- days in previous month
    daysPrevMonth = daysInMonth prevYear prevMonth
    -- days in previous month that should be shown with this month
    -- meaning days from monday to the day when this months starts
    -- if this month starts from monday then this is empty
    prevMonthDays = daysFromPreviousMonth prevYear prevMonth daysPrevMonth firstDay
    -- days in current month
    currentMonth = daysInMonth year month
    -- last day of the month (week day)
    lastDay = dayOfWeek $ fromGregorian year month currentMonth
    -- days in next month that should be shown with this month
    -- meaning days to sunday from the day when this month ends
    -- if this month ends in sunday then this is empty
    nextMonthDays = daysFromNextMonth nextYear nextMonth lastDay
    -- list of days in this month
    currentMonthDays = for [1..currentMonth] (fromGregorian year month)
    -- all the days together
    allDays = prevMonthDays ++ currentMonthDays ++ nextMonthDays
    -- rare case where in a leap year February starts in monday and
    -- all the days fit inside 4 week period so we need to add two additional weeks
    -- condition is smaller than 35 because we are aiming for 42 days (6*7) and
    -- 35 is 42 - 7 and I am too lazy to check if this is leap year and february
    howManyWeeks = if (length allDays < 35) then 2 else 1
    additionalWeeks = if (length allDays < 42)
      then [(length nextMonthDays)+1..(length nextMonthDays)+(7*howManyWeeks)]
      else []
  in
    allDays ++ (for additionalWeeks (fromGregorian nextYear nextMonth))

-- list of days from previous month depending on what day does
-- the current month start
-- if first day is 0 (Monday) then this will return empty list
-- if first day is 6 (Sunday) then this will return six days
-- numbered according to the previous month e.g. [26, 27, 28, 29, 30, 31]
daysFromPreviousMonth :: Integer -> Int -> Int -> Int -> [Day]
daysFromPreviousMonth year month days firstDay =
  reverse [(fromGregorian year month (days - i + 1)) | i <- [1..firstDay]]

-- list of days from next month depending on what day does
-- the current month end
-- if last day is 0 (Monday) then this will return six days [1, 2, 3, 4, 5, 6]
-- if last day is 6 (Sunday) then this will return empty list
daysFromNextMonth :: Integer -> Int -> Int -> [Day]
daysFromNextMonth year month lastDay =
  for [1..6-lastDay] (fromGregorian year month)

-- number of days in given month
daysInMonth :: Integer -> Int -> Int
daysInMonth y m = monthLength (isLeapYear y) m

-- 0 = Monday, 6 = Sunday
dayOfWeek :: Day -> Int
dayOfWeek day = (thd $ toWeekDate day) - 1

-- if january go to december and decrease year by one
prevMonthAndYear :: Integer -> Int -> (Integer, Int)
prevMonthAndYear year 1 = (year - 1, 12)
prevMonthAndYear year month = (year, month - 1)

-- if december go to january and increase year by one
nextMonthAndYear :: Integer -> Int -> (Integer, Int)
nextMonthAndYear year 12 = (year + 1, 1)
nextMonthAndYear year month = (year, month + 1)