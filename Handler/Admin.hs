module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
  Entity _ user <- requireAuth
  adminLayout $ do
    setTitleI MsgAdminPanel
    $(widgetFile "admin")