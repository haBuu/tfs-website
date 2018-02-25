module Model.PageMarkdown where

import Import

import Yesod.Markdown

import Model.Page

getCurrentVersionId :: Page -> DB (Maybe PageMarkdownId)
getCurrentVersionId page = do
  selectPage page >>= \case
    Just (Entity pid _) -> return $ Just pid
    Nothing -> return $ Nothing

getCurrentVersion :: Page -> DB Int
getCurrentVersion page = do
  selectPage page >>= \case
    Just (Entity _ pageMarkdown) -> return $ pageMarkdownVersion pageMarkdown
    Nothing -> return $ 0 -- this should not happen

getPageMarkdown :: Page -> DB Markdown
getPageMarkdown page = do
  selectPage page >>= \case
    Just (Entity _ pageMarkdown) -> return $ pageMarkdownContent pageMarkdown
    Nothing -> return $ Markdown ""

getPageMarkdownVersions :: Page -> DB [Entity PageMarkdown]
getPageMarkdownVersions page = selectList
  [ PageMarkdownPage ==. page
  , PageMarkdownVersion !=. 0 -- 0 is the empty initial version
  ] defaultOrder

emptyPageMarkdown :: Page -> UTCTime -> UserId -> PageMarkdown
emptyPageMarkdown page time userId =
  PageMarkdown page (Markdown "") time userId 0

selectPage page = selectFirst [PageMarkdownPage ==. page] defaultOrder
defaultOrder = [Desc PageMarkdownVersion, Desc PageMarkdownCreated]