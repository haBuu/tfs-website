module Handler.Courses where

import Import

import Helpers
import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getCoursesR :: Handler Html
getCoursesR = do
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Courses
  defaultLayout $ do
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "courses")
