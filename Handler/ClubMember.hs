module Handler.ClubMember where

import Import

import Yesod.Form.Bootstrap3

import Forms

getClubMembersR :: Handler Html
getClubMembersR = do
  notFound
  ((_, formWidget), formEnctype) <- runFormPost memberForm
  members <- runDB $ selectList [] [Desc ClubMemberName]
  defaultLayout $ do
    setTitleI MsgClubMembers
    $(widgetFile "club-members")

postClubMembersR :: Handler Html
postClubMembersR = do
  notFound
  ((result, _), _) <- runFormPost memberForm
  formHandler result $ \member -> do
    runDB $ insert_ member
    setMessageI MsgMemberAdded
  redirect ClubMembersR

deleteClubMemberR :: ClubMemberId -> Handler Html
deleteClubMemberR mid = do
  notFound
  runDB $ delete mid
  redirect ClubMembersR

memberForm :: Form ClubMember
memberForm extra = do
  mr <- getMessageRender
  (pdgaRes, pdgaView) <- mreq (checkPDGANumber intField)
    (withPlaceholder (mr MsgPDGANumber) $ bfs MsgPDGANumber) Nothing
  let result = ClubMember <$> pdgaRes <*> pure Nothing <*> pure Nothing
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel pdgaView}
          ^{fvInput pdgaView}
        <div .form-group>
          <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgAdd}>
      |]
  return (result, widget)

checkPDGANumber :: Field Handler Int -> Field Handler Int
checkPDGANumber field = checkBool
  (\v -> v >= 0) MsgNegativePDGANumber field