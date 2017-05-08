module Handler.Day where

import Import

import Data.Time.LocalTime

import qualified Model.Event as E
import Helpers
import Calendar
import Members

getDayR :: Day -> Handler Html
getDayR date = do
  events <- runDB $ E.getEventsDay date
  tz <- liftIO getCurrentTimeZone
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  defaultLayout $ do
    setTitle "Tampereen Frisbeeseura"
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "day")