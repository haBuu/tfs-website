module Handler.Event where

import Import

import Yesod.Markdown
import Yesod.Form.Bootstrap3
import Text.Julius(rawJS)

import Forms
import Helpers
import qualified Datepicker
import qualified Model.Post as P

-- how many events gets displayed
-- this will prevent admin from editing events
-- that are not among this limit but there should not
-- be any reason to edit such old events
eventLimit :: Int
eventLimit = 20

getEventsR :: Handler Html
getEventsR = do
  events <- runDB $ selectList [] [Desc EventCreated, LimitTo eventLimit]
  adminLayout $ do
    setTitleI MsgEditEvents
    mr <- getMessageRender
    $(widgetFile "drag-drop-image")
    $(widgetFile "events")

postEventR :: EventId -> Handler Html
postEventR eid = do
  event <- runDB $ get404 eid
  time <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost $ editEventForm time event
  formHandler result $ \(event, createPost) -> do
    if (checkDates (eventStartDate event) (eventEndDate event))
      then do
        runDB $ do
          replace eid event
          if createPost
            then
              updateEventPost eid event
            else
              case eventPost event of
                Nothing -> return ()
                Just pid -> do
                  update eid [EventPost =. Nothing]
                  delete pid
        setMessageI MsgEventEdited
      else
        setMessageI MsgEndDateInvalid
  redirect $ EditEventR eid

updateEventPost :: EventId -> Event -> DB ()
updateEventPost eid event = do
  case eventPost event of
    Just pid -> replace pid $ P.mkPostFromEvent event
    Nothing -> do
      pid <- insert $ P.mkPostFromEvent event
      update eid [EventPost =. Just pid]

deleteEventR :: EventId -> Handler Html
deleteEventR eid = do
  runDB $ delete eid
  redirect EventsR

getAddEventR :: Handler Html
getAddEventR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- runFormPost $ newEventForm uid time
  -- language for datepicker
  lang <- liftM language languages
  adminLayout $ do
    setTitleI MsgAddEvent
    mr <- getMessageRender
    Datepicker.addDatepicker
    $(widgetFile "datepicker")
    $(widgetFile "drag-drop-image")
    $(widgetFile "add-event")

postAddEventR :: Handler Html
postAddEventR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost $ newEventForm uid time
  formHandler result $ \(event, createPost) -> do
    if (checkDates (eventStartDate event) (eventEndDate event))
      then do
        runDB $ do
          eid <- insert event
          when createPost $ do
            pid <- insert $ P.mkPostFromEvent event
            update eid [EventPost =. Just pid]
        setMessageI MsgEventAdded
      else
        setMessageI MsgEndDateInvalid
  redirect AdminR

getEditEventR :: EventId -> Handler Html
getEditEventR eid = do
  event <- runDB $ get404 eid
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- runFormPost $ editEventForm time event
  -- language for datepicker
  lang <- liftM language languages
  adminLayout $ do
    setTitleI MsgEditEvent
    mr <- getMessageRender
    Datepicker.addDatepicker
    $(widgetFile "datepicker")
    $(widgetFile "drag-drop-image")
    $(widgetFile "delete-event")
    $(widgetFile "edit-event")

checkDates :: Day -> Maybe Day -> Bool
checkDates _ Nothing = True
checkDates start (Just end) = start < end

newEventForm :: UserId -> UTCTime -> Form (Event, Bool)
newEventForm uid time extra = do
  mr <- getMessageRender
  (titleRes, titleView) <- mreq textField
    (withPlaceholder (mr MsgTitle) $ bfs MsgTitle) Nothing
  (orgRes, orgView) <- mreq textField
    (withPlaceholder (mr MsgOrganizer) $ bfs MsgOrganizer) (Just "Tampereen Frisbeeseura")
  (contentRes, contentView) <- mreq markdownField
    (withRows "10" $ withPlaceholder (mr MsgEvent) $ bfs MsgEvent) Nothing
  (startRes, startView) <- mreq dayField
    (withPlaceholder (mr MsgStartDate) $ bfs MsgStartDate) Nothing
  (endRes, endView) <- mopt dayField
    (withPlaceholder (mr MsgEndDate) $ bfs MsgEndDate) Nothing
  (addPostRes, addPostView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAddPost) Nothing Nothing Nothing
      []) Nothing
  let eventRes = Event
                <$> pure uid
                <*> pure time
                <*> pure Nothing
                <*> titleRes
                <*> orgRes
                <*> contentRes
                <*> startRes
                <*> endRes
                <*> pure Nothing
  let result = (,) <$> eventRes <*> addPostRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel titleView}
          ^{fvInput titleView}
        <div .form-group>
          <label .control-label>^{fvLabel orgView}
          ^{fvInput orgView}
        <div .form-group>
          <label .control-label>^{fvLabel contentView}
          ^{fvInput contentView}
        <div .form-group>
          <label .control-label>^{fvLabel startView}
          <div .input-group .date>
            ^{fvInput startView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .control-label>^{fvLabel endView}
          <div .input-group .date>
            ^{fvInput endView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .c-input .c-checkbox>
            ^{fvInput addPostView}
            <span .c-indicator>
            ^{fvLabel addPostView}
        <div .form-group>
          <input type=submit .btn .btn-light .btn-block value=_{MsgAddEvent}>
      |]
  return (result, widget)

editEventForm :: UTCTime -> Event -> Form (Event, Bool)
editEventForm time event extra = do
  mr <- getMessageRender
  (titleRes, titleView) <- mreq textField
    (withPlaceholder (mr MsgTitle) $ bfs MsgTitle) (Just $ eventTitle event)
  (orgRes, orgView) <- mreq textField
    (withPlaceholder (mr MsgOrganizer) $ bfs MsgOrganizer) (Just $ eventOrganizer event)
  (contentRes, contentView) <- mreq markdownField
    (withRows "10" $ withPlaceholder (mr MsgEvent) $ bfs MsgEvent) (Just $ eventContent event)
  (startRes, startView) <- mreq dayField
    (withPlaceholder (mr MsgStartDate) $ bfs MsgStartDate) (Just $ eventStartDate event)
  (endRes, endView) <- mopt dayField
    (withPlaceholder (mr MsgEndDate) $ bfs MsgEndDate) (Just $ eventEndDate event)
  (addPostRes, addPostView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAddPost) Nothing Nothing Nothing
      []) (Just $ maybe False (const True) (eventPost event))
  let eventRes = Event
                <$> pure (eventUser event)
                <*> pure (eventCreated event)
                <*> pure (Just time)
                <*> titleRes
                <*> orgRes
                <*> contentRes
                <*> startRes
                <*> endRes
                <*> pure (eventPost event)
  let result = (,) <$> eventRes <*> addPostRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel titleView}
          ^{fvInput titleView}
        <div .form-group>
          <label .control-label>^{fvLabel orgView}
          ^{fvInput orgView}
        <div .form-group>
          <label .control-label>^{fvLabel contentView}
          ^{fvInput contentView}
        <div .form-group>
          <label .control-label>^{fvLabel startView}
          <div .input-group .date>
            ^{fvInput startView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .control-label>^{fvLabel endView}
          <div .input-group .date>
            ^{fvInput endView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .c-input .c-checkbox>
            ^{fvInput addPostView}
            <span .c-indicator>
            ^{fvLabel addPostView}
        <div .form-group>
          <input type=submit .btn .btn-light .btn-block value=_{MsgSave}>
      |]
  return (result, widget)