{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Data.Kind (Type)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation

-- TODO: remove yesod-nic

import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
-- TODO: remove when not needed anymore
-- import Yesod.Auth.Dummy

import Network.Mail.Mime hiding (htmlPart)
import Text.Shakespeare.Text (stext)
import Yesod.Auth.Email
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- TODO: change login button to logout when login'd

{- | The foundation datatype for your application. This can be a good place to
 keep settings and values requiring initialization before your application
 starts running, such as database connections. Every handler will have
 access to the data present here.
-}
data App = App
  { appSettings :: AppSettings
  , appStatic :: Static
  -- ^ Settings for static file serving.
  , appConnPool :: ConnectionPool
  -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

-- add translation support, sadly I have to chose a default one, and i choose french, but I will try not to discriminate
mkMessage "App" "messages" "fr"

data MenuItem = MenuItem
  { menuItemLabel :: AppMessage
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  , menuItemIcon :: Text
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarTop MenuItem

data ThemeItem = ThemeItem
  { themeItemId :: Text
  , themeItemIcon :: Text
  }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: Type -> Type).
  (MonadUnliftIO m) =>
  ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: (ToTypedContent res) => Handler res -> Handler res
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs

    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft
              $ MenuItem
                { menuItemLabel = MsgHome
                , menuItemRoute = HomeR
                , menuItemAccessCallback = True
                , menuItemIcon = "home"
                }
          , NavbarLeft
              $ MenuItem
                { menuItemLabel = MsgNews
                , menuItemRoute = NewsR
                , menuItemAccessCallback = True
                , menuItemIcon = "newspaper"
                }
          , NavbarLeft
              $ MenuItem
                { menuItemLabel = MsgCalendar
                , menuItemRoute = ProfileR
                , menuItemAccessCallback = True
                , menuItemIcon = "calendar_month"
                }
          , NavbarLeft
              $ MenuItem
                { menuItemLabel = MsgFiles
                , menuItemRoute = ProfileR
                , menuItemAccessCallback = True
                , menuItemIcon = "description"
                }
          , NavbarTop
              $ MenuItem
                { menuItemLabel = MsgLogin
                , menuItemRoute = AuthR LoginR
                , menuItemAccessCallback = isNothing muser
                , menuItemIcon = "login"
                }
          , NavbarTop
              $ MenuItem
                { menuItemLabel = MsgLogout
                , menuItemRoute = AuthR LogoutR
                , menuItemAccessCallback = isJust muser
                , menuItemIcon = "logout"
                }
          ]

    let themeItems =
          [ ThemeItem{themeItemId = "lightIconId", themeItemIcon = "light_mode"}
          , ThemeItem{themeItemId = "darkIconId", themeItemIcon = "dark_mode"}
          ]

    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarTopMenuItems = [x | NavbarTop x <- menuItems]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarTopMenuItems, menuItemAccessCallback x]

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      mtoken <- fmap reqToken getRequest
      $(widgetFile "default-layout")
      $(widgetFile "wrap-image")
      $(widgetFile "tabs")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute ::
    App ->
    Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized ::
    Route App -> -- \^ The route the user is visiting.
    Bool -> -- \^ Whether or not this is a "write" request.
    Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized SwitchLangR _ = return Authorized
  isAuthorized NewsR _ = return Authorized
  isAuthorized (NewsEntryR _) _ = return Authorized
  -- routes that need to be authenticated (every member can access it)
  isAuthorized ProfileR _ = isAuthenticated
  -- routes for admins only TODO: fix this
  isAuthorized (EditNewsEntryR _) _ = isAuthenticatedAsAdmin
  isAuthorized NewNewsEntryR _ = isAuthenticatedAsAdmin

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    Text -> -- \^ The file extension
    Text -> -- \^ The MIME content type
    LByteString -> -- \^ The contents of the file
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
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
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return
      $ appShouldLogAll (appSettings app)
      || level
      == LevelWarn
      || level
      == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  -- Takes the route that the user is currently on, and returns a tuple
  -- of the 'Text' that you want the label to display, and a previous
  -- breadcrumb route.
  breadcrumb ::
    Route App -> -- \^ The route the user is visiting currently.
    Handler (Text, Maybe (Route App))
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR = return ("Profile", Just HomeR)
  breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = HomeR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Creds App ->
    m (AuthenticationResult App)
  authenticate creds = liftHandler $ runDB $ do
    -- TODO: we might in our case not want to allow creating new user, check this in more details later
    x <- getBy $ UniqueUser $ credsIdent creds
    case x of
      Just (Entity uid _) -> return $ Authenticated uid
      Nothing ->
        Authenticated
          <$> insert
            User
              { userEmail = credsIdent creds
              , userPassword = Nothing
              , userVerkey = Nothing
              , userVerified = False
              }

  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app = [authEmail]

--  : extraAuthPlugins
-- where
-- Enable authDummy login if enabled.
--  extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page" -- TODO: i18n
    Just _ -> Authorized

isAuthenticatedAsAdmin :: Handler AuthResult
isAuthenticatedAsAdmin = do
  muser <- maybeAuth
  return $ case muser of
    Nothing -> Unauthorized "You must login to access this page" -- TODO: i18n
    Just (Entity _ user) -> if userEmail user == "a@a.com" then Authorized else Unauthorized "You must be admin to access this page" -- TODO: check if uid is in admin list instead of hardcoded credentials

-- TODO: change this one for security reasons once prototype is done, maybe make it match with the one on top
isAdmin :: Maybe (Entity User) -> Bool
isAdmin muser = case muser of
  Nothing -> False
  Just (Entity _ user) -> userEmail user == "a@a.com"

instance YesodAuthPersist App

instance YesodAuthEmail App where
  type AuthEmailId App = UserId

  afterPasswordRoute _ = HomeR

  addUnverified email verkey =
    liftHandler $ runDB $ insert $ User email Nothing (Just verkey) False

  sendVerifyEmail email _ verurl = do
    -- Print out to the console the verification email, for easier
    -- debugging.
    liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" ++ verurl

    -- Send email.
    liftIO
      $ renderSendMail
        (emptyMail $ Address Nothing "noreply")
          { mailTo = [Address Nothing email]
          , mailHeaders =
              [ ("Subject", "Verify your email address")
              ]
          , mailParts = [[textPart, htmlPart]]
          }
   where
    textPart =
      Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = None
        , partDisposition = DefaultDisposition
        , partContent =
            PartContent
              $ encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
        , partHeaders = []
        }
    html =
      encodeUtf8
        [stext|
                    <p>Please confirm your email address by clicking on the link below.</p>
                    <p>
                        <a href=#{verurl}>#{verurl}
                    </p>
                    <p>Thank you</p>
                    |]

    htmlPart =
      Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partDisposition = DefaultDisposition
        , partContent =
            PartContent html
        , partHeaders = []
        }
  getVerifyKey = liftHandler . runDB . fmap (userVerkey =<<) . get
  setVerifyKey uid key = liftHandler $ runDB $ update uid [UserVerkey =. Just key]
  verifyAccount uid = liftHandler $ runDB $ do
    mu <- get uid
    case mu of
      Nothing -> return Nothing
      Just u -> do
        update uid [UserVerified =. True, UserVerkey =. Nothing]
        return $ Just uid
  getPassword = liftHandler . runDB . fmap (userPassword =<<) . get
  setPassword uid pass = liftHandler . runDB $ update uid [UserPassword =. Just pass]
  getEmailCreds email = liftHandler $ runDB $ do
    mu <- getBy $ UniqueUser email
    case mu of
      Nothing -> return Nothing
      Just (Entity uid u) ->
        return
          $ Just
            EmailCreds
              { emailCredsId = uid
              , emailCredsAuthId = Just uid
              , emailCredsStatus = isJust $ userPassword u
              , emailCredsVerkey = userVerkey u
              , emailCredsEmail = email
              }
  getEmail = liftHandler . runDB . fmap (fmap userEmail) . get

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

isHtmxRequest :: Handler Bool
isHtmxRequest = do
  maybeHeader <- lookupHeader "HX-Request"
  return $ isJust maybeHeader

-- this function has been modified to seamlessly support htmx, either returns whole page, or just the requested part if htmx is supported
renderWidget :: AppMessage -> Widget -> Handler Html
renderWidget message widget = do
  htmx <- isHtmxRequest
  if htmx
    then do
      pageContent <- widgetToPageContent widget
      withUrlRenderer $ pageBody pageContent
    else defaultLayout $ do
      setTitleI message
      widget

data TabItem = TabItem
  { tabItemLabel :: AppMessage
  , tabItemId :: Text
  , tabItemContent :: Widget
  , isChecked :: Bool
  }

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
