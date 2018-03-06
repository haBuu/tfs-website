module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Message
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Yesod.Auth.HashDB (HashDBUser(..))
import Yesod.Auth.HashDB (authHashDBWithForm)

import qualified ErrorHandlers as E

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static -- ^ Settings for static file serving.
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  , appFiles       :: Static
  }

instance HasHttpManager App where
  getHttpManager = appHttpManager

-- Set up i18n messages. See the message folder.
-- yesod will overwrite this if it detects finnish
mkMessage "App" "messages" "fi"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- | A convenient synonym for database action.
type DB a = ReaderT SqlBackend Handler a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot = ApprootMaster $ appRoot . appSettings

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    (60 * 24 * 30 * 3) -- timeout in minutes, three months
    "config/client_session_key.aes"

  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuth
    pages <- runDB $ selectList [PageTopLevel ==. True, PageObsolete ==. False] []

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      addStylesheetRemote "//maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
      addScriptRemote "//code.jquery.com/jquery-3.2.1.min.js"
      addScriptRemote "//maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"
      $(widgetFile "style")
      $(widgetFile "header")
      $(widgetFile "message")
      $(widgetFile "default-layout")
      setTitle "Tampereen Frisbeeseura"
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR

  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized (ImagesR _) _ = return Authorized

  -- public
  isAuthorized HomeR _ = return Authorized
  isAuthorized (OnePostR _) _ = return Authorized
  isAuthorized (HomePageR _) _ = return Authorized
  isAuthorized (PageR _) _ = return Authorized

  -- admin
  isAuthorized AdminR _ = isAdmin
  isAuthorized PostsR _ = isAdmin
  isAuthorized AddPostR _ = isAdmin
  isAuthorized (EditPostR _) _ = isAdmin
  isAuthorized (PostR _) _ = isAdmin
  isAuthorized ImageR _ = isAdmin
  isAuthorized PagesR _ = isAdmin
  isAuthorized (EditPageR _ _) _ = isAdmin
  isAuthorized AddPageR _ = isAdmin

  -- super admin
  isAuthorized AddUserR _ = isSuperAdmin
  isAuthorized UsersR _ = isSuperAdmin
  isAuthorized (UserR _) _ = isSuperAdmin

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
        minifym
        genFileName
        staticDir
        (StaticR . flip StaticRoute [])
        ext
        mime
        content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = return . appLogger

  errorHandler = E.defaultErrorHandler

isAdmin :: Handler AuthResult
isAdmin = do
  mr <- getMessageRender
  user <- liftM entityVal requireAuth
  return $ if userAdmin user
    then Authorized
    else Unauthorized $ mr MsgNotAdmin

isSuperAdmin :: Handler AuthResult
isSuperAdmin = do
  mr <- getMessageRender
  user <- liftM entityVal requireAuth
  return $ if userSuperAdmin user
    then Authorized
    else Unauthorized $ mr MsgNotSuperAdmin

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  renderAuthMessage _ [] = defaultMessage
  renderAuthMessage _ ("en":_) = defaultMessage
  renderAuthMessage _ ("fi":_) = finnishMessage
  renderAuthMessage m (_:ls) = renderAuthMessage m ls

  -- Where to send a user after successful login
  loginDest _ = HomeR
  -- Where to send a user after logout
  logoutDest _ = HomeR
  -- Override the above two destinations when a Referer: header is present
  redirectToReferer _ = False

  authenticate creds = runDB $ do
    x <- getBy $ UniqueUser $ credsIdent creds
    return $ case x of
      Just (Entity uid _) -> Authenticated uid
      Nothing -> UserError InvalidLogin

  -- You can add other plugins like BrowserID, email or OAuth here
  authPlugins _ = [authHashDBWithForm myLoginForm (Just . UniqueUser)]

  authHttpManager = getHttpManager

  authLayout = defaultLayout

instance HashDBUser User where
  userPasswordHash = userPassword
  setPasswordHash h u = u { userPassword = Just h }

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

myLoginForm :: Route App -> Widget
myLoginForm action = do
  [whamlet|
    <div .container .mt-3>
      <form method=post action=@{action}>
        <fieldset .form-group>
          <label>_{MsgName}
          <input type=text name=username .form-control placeholder=_{MsgName}>
        <fieldset .form-group>
          <label>_{MsgPassword}
          <input type=password name=password .form-control placeholder=_{MsgPassword}>
        <fieldset .form-group>
          <button type=submit .btn .btn-light .btn-block>_{MsgLogin}
  |]