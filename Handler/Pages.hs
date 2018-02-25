module Handler.Pages where

import Import

import Data.Time (getCurrentTimeZone)

import Yesod.Markdown

import Model.Page
import Model.PageMarkdown
import PageMessage
import Helpers

getPagesR :: Handler Html
getPagesR = do
  Entity userId user <- requireAuth
  pages <- forM [minBound..maxBound] $ \page -> runDB $ do
    maybeId <- getCurrentVersionId page
    case maybeId of
      Just pageId -> return (pageId, page)
      Nothing -> do
        -- add empty page since the page did not exists
        time <- liftIO getCurrentTime
        pageId <- insert $ emptyPageMarkdown page time userId
        return (pageId, page)
  adminLayout $ do
    setTitleI MsgPages
    $(widgetFile "pages")

getEditPageR :: PageMarkdownId -> Handler Html
getEditPageR pageId = do
  Entity userId user <- requireAuth
  pageMarkdown <- runDB $ get404 pageId
  versions <- runDB $ getPageMarkdownVersions $ pageMarkdownPage pageMarkdown
  tz <- liftIO getCurrentTimeZone
  adminLayout $ do
    setTitleI MsgEditPage
    addScript $ StaticR js_markdown_js
    let markdownPreview = $(widgetFile "markdown-preview")
    $(widgetFile "markdown")
    $(widgetFile "edit-page")

postEditPageR :: PageMarkdownId -> Handler Html
postEditPageR pageId = do
  Entity userId _ <- requireAuth
  pageMarkdown <- runDB $ get404 pageId
  version <- runDB $ getCurrentVersion $ pageMarkdownPage pageMarkdown
  time <- liftIO getCurrentTime
  result <- runInputPost $ PageMarkdown
    <$> pure (pageMarkdownPage pageMarkdown)
    <*> ireq markdownField "markdown"
    <*> pure time
    <*> pure userId
    <*> pure (version + 1)
  runDB $ insert result
  redirect PagesR