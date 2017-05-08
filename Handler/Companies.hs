module Handler.Companies where

import Import

import Helpers
import Members
import Calendar
import Model.Event
import Model.Page
import Model.PageMarkdown

getCompaniesR :: Handler Html
getCompaniesR = do
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Companies
  defaultLayout $ do
    setTitleI MsgCompanies
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "companies")