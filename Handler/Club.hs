module Handler.Club where

import Import

import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getClubR :: Handler Html
getClubR = do
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Club
  defaultLayout $ do
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "club")