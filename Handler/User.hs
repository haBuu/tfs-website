module Handler.User where

import Import

import Yesod.Form.Bootstrap3
import Yesod.Auth.HashDB(setPassword)

import Forms
import Data.Maybe

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Asc UserName]
  defaultLayout $ do
    $(widgetFile "users")

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ get404 uid
  ((_, formWidget), formEnctype) <- runFormPost $ editUserForm user
  defaultLayout $ do
    $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
  user <- runDB $ get404 uid
  ((result, _), _) <- runFormPost $ editUserForm user
  formHandler result $ \res -> do
    runDB $ replace uid res
    setMessageI MsgUserUpdated
  redirect $ UserR uid

editUserForm :: User -> Form User
editUserForm user extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) (Just $ userName user)
  (adminRes, adminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAdmin) Nothing Nothing Nothing
      [("class", "form-check-input")]) (Just $ userAdmin user)
  (superAdminRes, superAdminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgSuperAdmin) Nothing Nothing Nothing
      [("class", "form-check-input")]) (Just $ userSuperAdmin user)
  let result = User <$> nameRes
                    <*> pure (userPassword user)
                    <*> adminRes
                    <*> superAdminRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-check>
          <label .form-check-label>
            ^{fvInput adminView} ^{fvLabel adminView}
        <div .form-check>
          <label .form-check-label>
            ^{fvInput superAdminView} ^{fvLabel superAdminView}
        <div .form-group .mt-3>
          <input type=submit .btn .btn-light .btn-block value=_{MsgSave}>
      |]
  return (result, widget)

getAddUserR :: Handler Html
getAddUserR = do
  ((_, formWidget), formEnctype) <- runFormPost newUserForm
  defaultLayout $ do
    setTitleI MsgAddUser
    $(widgetFile "add-user")

postAddUserR :: Handler Html
postAddUserR = do
  ((result, _), _) <- runFormPost newUserForm
  formHandler result $ \user -> do
    runDB $ insert_ =<< setPassword (fromJust $ userPassword user) user
    setMessageI MsgUserAdded
  redirect UsersR

newUserForm :: Form User
newUserForm extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (pwRes, pwView) <- mopt textField
    (withPlaceholder (mr MsgPassword) $ bfs MsgPassword) Nothing
  (adminRes, adminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAdmin) Nothing Nothing Nothing
      [("class", "form-check-input")]) Nothing
  (superAdminRes, superAdminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgSuperAdmin) Nothing Nothing Nothing
      [("class", "form-check-input")]) Nothing
  let result = User <$> nameRes
                    <*> pwRes
                    <*> adminRes
                    <*> superAdminRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .form-check>
          <label .form-check-label>
            ^{fvInput adminView} ^{fvLabel adminView}
        <div .form-check>
          <label .form-check-label>
            ^{fvInput superAdminView} ^{fvLabel superAdminView}
        <div .form-group .mt-3>
          <input type=submit .btn .btn-light .btn-block value=_{MsgAddUser}>
      |]
  return (result, widget)