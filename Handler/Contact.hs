module Handler.Contact where

import Import

import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getContactR :: Handler Html
getContactR = do
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Contact
  defaultLayout $ do
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "contact")
