module Model.PageMarkdown where

import Import

import Yesod.Markdown

import Model.Page

getPageMarkdown :: Page -> DB Markdown
getPageMarkdown page = do
  maybePageMarkdown <- selectFirst [PageMarkdownPage ==. page] []
  return $ case maybePageMarkdown of
    Just (Entity _ pageMarkdown) -> pageMarkdownContent pageMarkdown
    Nothing -> Markdown ""

emptyPageMarkdown :: Page -> UTCTime -> UserId -> PageMarkdown
emptyPageMarkdown page time userId =
  PageMarkdown page (Markdown "") time userId Nothing Nothing