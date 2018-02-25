module Handler.Club where

import Import

import Yesod.Form.Bootstrap3

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding

import Forms
import Helpers
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