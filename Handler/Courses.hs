module Handler.Courses where

import Import

import Helpers
import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getCoursesR :: Handler Html
getCoursesR = do
  members <- membersWidget
  widget <- calendarWidget
  content <- runDB $ getPageMarkdown Courses
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "courses")
