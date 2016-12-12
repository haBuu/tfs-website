module Handler.Competitions where

import Import

import Helpers
import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getCompetitionsR :: Handler Html
getCompetitionsR = do
  members <- membersWidget
  widget <- calendarWidget
  content <- runDB $ getPageMarkdown Competitions
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "competitions")