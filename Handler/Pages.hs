module Handler.Pages where

import Import

import Yesod.Form.Bootstrap3
import Yesod.Markdown

import Text.Julius(rawJS)
import Data.Time (getCurrentTimeZone)

import Model.Page
import Model.PageMarkdown
import Helpers
import Forms

getPageR :: Text -> Handler Html
getPageR name = do
  pid <- runDB $ selectFirst [PageName ==. name, PageObsolete ==. False] [] >>= \case
    Nothing -> notFound
    Just (Entity pid _page) -> return pid
  pageContent <- runDB $ getPageContent pid
  defaultLayout $ do
    $(widgetFile "banner")
    $(widgetFile "page")

getPagesR :: Handler Html
getPagesR = do
  ((_, formWidget), formEnctype) <- runFormPost newPageForm
  pages <- runDB $ selectList [] []
  let toplevelPages = filter (topLevelFilter . entityVal) pages
  let otherPages = filter (otherFilter . entityVal) pages
  let obsoletePages = filter (obsoleteFilter . entityVal) pages
  defaultLayout $ do
    setTitleI MsgPages
    $(widgetFile "pages")

postAddPageR :: Handler Html
postAddPageR = do
  ((result, _), _) <- runFormPost newPageForm
  formHandler result $ \page -> do
    void $ runDB $ insert page
    setMessageI MsgPageAdded
  redirect PagesR

newPageForm :: Form Page
newPageForm extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (topRes, topView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgTopLevel) Nothing Nothing Nothing
      [("class", "form-check-input")]) Nothing
  let result = Page <$> nameRes
                    <*> topRes
                    <*> pure 0
                    <*> pure False
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-check>
          <label .form-check-label>
            ^{fvInput topView} ^{fvLabel topView}
        <div .form-group .mt-3>
          <input type=submit .btn .btn-light .btn-block value=_{MsgAddPage}>
      |]
  return (result, widget)

getEditPageR :: PageId -> Int -> Handler Html
getEditPageR pid version = do
  page <- runDB $ get404 pid
  maybePageContent <- if version == 0
    then runDB $ selectFirst [PageContentPage ==. pid] defaultOrder
    else runDB $ selectFirst [PageContentPage ==. pid, PageContentVersion ==. version] []
  versions <- runDB $ getPageMarkdownVersions pid
  mr <- getMessageRender
  tz <- liftIO getCurrentTimeZone
  defaultLayout $ do
    addScript $ StaticR js_markdown_js
    let markdownPreview = $(widgetFile "markdown-preview")
    $(widgetFile "markdown")
    $(widgetFile "drag-drop-image")
    $(widgetFile "delete-page")
    $(widgetFile "edit-page")

postEditPageR :: PageId -> Int -> Handler Html
postEditPageR pid _version = do
  Entity userId _ <- requireAuth
  version <- runDB $ getCurrentVersion pid
  time <- liftIO getCurrentTime
  result <- runInputPost $ PageContent
    <$> pure pid
    <*> ireq markdownField "markdown"
    <*> pure time
    <*> pure userId
    <*> pure (version + 1)
  void $ runDB $ insert result
  redirect $ EditPageR pid 0

-- Pages are never removed just marked obsolete
deleteEditPageR :: PageId -> Int -> Handler Html
deleteEditPageR pid _version = do
  runDB $ update pid [PageObsolete =. True]
  redirect PagesR

putEditPageR :: PageId -> Int -> Handler Html
putEditPageR pid _version = do
  runDB $ update pid [PageObsolete =. False]
  redirect PagesR