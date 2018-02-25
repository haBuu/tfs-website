module Model.PageMarkdown where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Yesod.Markdown

import Model.Page

getCurrentVersionId :: Page -> DB (Maybe PageMarkdownId)
getCurrentVersionId page = do
  selectPage page >>= \case
    Just (Entity pid _) -> return $ Just pid
    Nothing -> return Nothing

getCurrentVersion :: Page -> DB Int
getCurrentVersion page = do
  selectPage page >>= \case
    Just (Entity _ pageMarkdown) -> return $ pageMarkdownVersion pageMarkdown
    Nothing -> return 0 -- this should not happen

getPageMarkdown :: Page -> DB Markdown
getPageMarkdown page = do
  selectPage page >>= \case
    Just (Entity _ pageMarkdown) -> return $ pageMarkdownContent pageMarkdown
    Nothing -> return $ Markdown ""

getPageMarkdownVersions :: Page -> DB [(Entity PageMarkdown, Entity User)]
getPageMarkdownVersions page = E.select $
  E.from $ \(pageMarkdown, user) -> do
    E.where_ $ pageMarkdown ^. PageMarkdownPage E.==. E.val page
    E.where_ $ pageMarkdown ^. PageMarkdownUser E.==. user ^. UserId
    -- 0 is the empty initial version so exclude that
    E.where_ $ pageMarkdown ^. PageMarkdownVersion E.!=. E.val 0
    E.orderBy [E.desc (pageMarkdown ^. PageMarkdownVersion)]
    E.orderBy [E.desc (pageMarkdown ^. PageMarkdownCreated)]
    return (pageMarkdown, user)

emptyPageMarkdown :: Page -> UTCTime -> UserId -> PageMarkdown
emptyPageMarkdown page time userId =
  PageMarkdown page (Markdown "") time userId 0

selectPage page = selectFirst [PageMarkdownPage ==. page] defaultOrder
defaultOrder = [Desc PageMarkdownVersion, Desc PageMarkdownCreated]