module Handler.Competitions where

import Import

import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getCompetitionsR :: Handler Html
getCompetitionsR = do
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Competitions
  defaultLayout $ do
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "competitions")