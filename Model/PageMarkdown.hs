module Model.PageMarkdown where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Yesod.Markdown

getCurrentVersionId :: PageId -> DB (Maybe PageContentId)
getCurrentVersionId pid = do
  selectPage pid >>= \case
    Just (Entity pcid _) -> return $ Just pcid
    Nothing -> return Nothing

getCurrentVersion :: PageId -> DB Int
getCurrentVersion pid = do
  selectPage pid >>= \case
    Just (Entity _ pageContent) -> return $ pageContentVersion pageContent
    Nothing -> return 0

getPageContent :: PageId -> DB Markdown
getPageContent pid = do
  selectPage pid >>= \case
    Just (Entity _ pageContent) -> return $ pageContentContent pageContent
    Nothing -> return $ Markdown ""

getPageMarkdownVersions :: PageId -> DB [(Entity PageContent, Entity User)]
getPageMarkdownVersions pid = E.select $
  E.from $ \(pageContent, user) -> do
    E.where_ $ pageContent ^. PageContentPage E.==. E.val pid
    E.where_ $ pageContent ^. PageContentUser E.==. user ^. UserId
    E.orderBy [E.desc (pageContent ^. PageContentVersion)]
    E.orderBy [E.desc (pageContent ^. PageContentCreated)]
    return (pageContent, user)

emptyPageContent :: PageId -> UTCTime -> UserId -> PageContent
emptyPageContent pid time userId =
  PageContent pid (Markdown "") time userId 0

selectPage pid = selectFirst [PageContentPage ==. pid] defaultOrder
defaultOrder = [Desc PageContentVersion, Desc PageContentCreated]