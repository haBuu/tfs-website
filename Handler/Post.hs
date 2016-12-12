module Handler.Post where

import Import

import Yesod.Markdown
import Yesod.Form.Bootstrap3
import Data.Time.LocalTime
import Text.Julius(rawJS)

import Helpers
import Model.Post

-- how many posts gets displayed
-- this will prevent admin from editing posts
-- that are not among this limit but there should not
-- be any reason to edit such old posts
postLimit :: Int
postLimit = 50

getPostsR :: Handler Html
getPostsR = do
  posts <- runDB $ posts postLimit
  tz <- liftIO getCurrentTimeZone
  defaultLayout $ do
    setTitleI MsgEditPosts
    $(widgetFile "posts")

getAddPostR :: Handler Html
getAddPostR = do
  uid <- requireAuthId
  defaultLayout $ do
    setTitleI MsgAddPost
    mr <- getMessageRender
    addScript $ StaticR js_markdown_js
    let markdownPreview = $(widgetFile "markdown-preview")
    $(widgetFile "markdown")
    $(widgetFile "drag-drop-image")
    $(widgetFile "add-post")

postAddPostR :: Handler Html
postAddPostR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  result <- runInputPost $ Post
    <$> pure time
    <*> pure uid
    <*> pure Nothing
    <*> pure Nothing
    <*> ireq textField "title"
    <*> ireq markdownField "markdown"
  runDB $ insert_ result
  setMessageI MsgPostAdded
  redirect AdminR

postPostR :: PostId -> Handler Html
postPostR pid = do
  uid <- requireAuthId
  post <- runDB $ get404 pid
  time <- liftIO getCurrentTime
  result <- runInputPost $ Post
    <$> pure (postCreated post)
    <*> pure (postUserCreated post)
    <*> pure (Just time)
    <*> pure (Just uid)
    <*> ireq textField "title"
    <*> ireq markdownField "markdown"
  liftIO $ print result
  runDB $ replace pid result
  setMessageI MsgPostEdited
  redirect $ EditPostR pid

deletePostR :: PostId -> Handler Html
deletePostR pid = do
  runDB $ delete pid
  redirect PostsR

getEditPostR :: PostId -> Handler Html
getEditPostR pid = do
  post <- runDB $ get404 pid
  defaultLayout $ do
    setTitleI MsgEditPost
    mr <- getMessageRender
    addScript $ StaticR js_markdown_js
    let markdownPreview = $(widgetFile "markdown-preview")
    $(widgetFile "markdown")
    $(widgetFile "drag-drop-image")
    $(widgetFile "delete-post")
    $(widgetFile "edit-post")