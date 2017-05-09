module Handler.Pages where

import Import

import Yesod.Markdown

import Model.Page
import Model.PageMarkdown
import PageMessage

getPagesR :: Handler Html
getPagesR = do
  Entity userId user <- requireAuth
  pages <- forM [minBound..maxBound] $ \page -> runDB $ do
    mPageMarkdown <- selectFirst [PageMarkdownPage ==. page] []
    case mPageMarkdown of
      Just (Entity pageId _) -> return (pageId, page)
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
  time <- liftIO getCurrentTime
  result <- runInputPost $ PageMarkdown
    <$> pure (pageMarkdownPage pageMarkdown)
    <*> ireq markdownField "markdown"
    <*> pure (pageMarkdownCreated pageMarkdown)
    <*> pure (pageMarkdownUserCreated pageMarkdown)
    <*> pure (Just time)
    <*> pure (Just userId)
  runDB $ repsert pageId result
  redirect PagesR