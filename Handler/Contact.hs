module Handler.Contact where

import Import

import Helpers
import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getContactR :: Handler Html
getContactR = do
  members <- membersWidget
  widget <- calendarWidget
  content <- runDB $ getPageMarkdown Contact
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "contact")
