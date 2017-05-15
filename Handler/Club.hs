module Handler.Club where

import Import

import Yesod.Form.Bootstrap3

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding

import Forms
import Helpers
import Calendar
import Members
import Model.Page
import Model.PageMarkdown

getClubR :: Handler Html
getClubR = do
  (year, _, _) <- liftM (toGregorian . utctDay) $ liftIO getCurrentTime
  let formDays, formMonths, formYears :: [Int]
      formDays = [1..31]
      formMonths = [1..12]
      formYears = [1900..fromIntegral year]
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Club
  defaultLayout $ do
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "club")

postClubR :: Handler Html
postClubR = do
  result <- runInputPost $ Member
    <$> ireq textField "firstname"
    <*> ireq textField "lastname"
    <*> ireq textField "address"
    <*> ireq intField "zip-code"
    <*> ireq textField "city"
    <*> ireq textField "phone"
    <*> ireq textField "email"
    <*> ireq (radioFieldList sexOptions) "sex"
    <*> ireq (selectFieldList dayOptions) "day"
    <*> ireq (selectFieldList monthOptions) "month"
    <*> ireq (selectFieldList yearOptions) "year"
    <*> iopt intField "pdga"
    <*> ireq (radioFieldList membershipOptions) "membership"
    <*> ireq (radioFieldList licenseOptions) "license"
    <*> ireq checkBoxField "disc"
    <*> ireq checkBoxField "magazine"
    <* ireq (checkCheck textField) "check"
  liftIO $ sendJoinMail result
  liftIO $ sendPaymentInfoMail result
  redirect ClubR

sender :: Text -> Address
sender from = Address (Just "Tampereen Frisbeeseura") from

sendPaymentInfoMail :: Member -> IO ()
sendPaymentInfoMail member = do
  liftIO $ renderSendMail (emptyMail $ sender "jasenvastaava@tfs.fi")
    { mailTo = [Address Nothing (email member)]
    , mailHeaders =
        [ ("Subject", "Liittyminen Tampereen Frisbeeseuraan")
        ]
    , mailParts = [[textPart, htmlPart]]
    }
  where
    name = firstName member ++ " " ++ lastName member
    textPart = Part
      { partType = "text/plain; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = Data.Text.Lazy.Encoding.encodeUtf8
          [stext|
            Tilinumero: FI17 8330 0710 4436 19
            Saaja: Tampereen Frisbeeseura
            Viestiin: Jäsenmaksu 2017, #{name}.
            Summa: #{show $ countSum member} €
          |]
      , partHeaders = []
      }
    htmlPart = Part
      { partType = "text/html; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = renderHtml
          [shamlet|
            <p>Tilinumero: FI17 8330 0710 4436 19
            <p>Saaja: Tampereen Frisbeeseura
            <p>Viestiin: Jäsenmaksu 2017, #{name}.
            <p>Summa: #{show $ countSum member} €
          |]
      , partHeaders = []
      }

sendJoinMail :: Member -> IO ()
sendJoinMail member@Member {..} = do
  let name = firstName ++ " " ++ lastName
  liftIO $ renderSendMail (emptyMail $ sender "lomake@tfs.fi")
    { mailTo = [Address Nothing "jasenvastaava@tfs.fi"]
    , mailHeaders =
        [ ("Subject", "Seuraan liittyminen, " ++ name)
        , ("Reply-To", email)
        ]
    , mailParts = [[textPart, htmlPart]]
    }
  where
    name = firstName ++ " " ++ lastName
    textPart = Part
      { partType = "text/plain; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = Data.Text.Lazy.Encoding.encodeUtf8
          [stext|
            #{name}
            #{address}
            #{show zipCode}
            #{city}
            #{phone}
            #{email}
            #{showSex sex}
            #{dobDay}.#{dobMonth}.#{dobYear}
            #{showPdga mpdga}
            #{showMembership membership}
            #{showLicense membership license}
            $if disc
              Jäsenkiekko: Kyllä
            $else
              Jäsenkiekko: Ei
            $if magazine
              Discgolfer-lehti: Kyllä
            $else
              Discgolfer-lehti: Ei
            Summa: #{show $ countSum member} €
          |]
      , partHeaders = []
      }
    htmlPart = Part
      { partType = "text/html; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = renderHtml
          [shamlet|
            <p>#{show member}
          |]
      , partHeaders = []
      }

showMembership :: Membership -> String
showMembership Adult = "Jäsenmaksu, aikuinen"
showMembership Junior = "Jäsenmaksu, juniori"

showLicense :: Membership -> License -> String
showLicense _ NoLicense = "Ei lisenssiä"
showLicense Adult A = "A-lisenssi, aikuinen"
showLicense Adult B = "B-lisenssi, aikuinen"
showLicense Junior A = "A-lisenssi, juniori"
showLicense Junior B = "B-lisenssi, juniori"

showPdga :: Maybe Int -> String
showPdga Nothing = "PDGA-numero: -"
showPdga (Just pdga) = "PDGA-numero: " ++ show pdga

showSex :: Sex -> String
showSex Male = "Mies"
showSex Female = "Nainen"

data Member = Member
  { firstName :: Text
  , lastName :: Text
  , address :: Text
  , zipCode :: Int
  , city :: Text
  , phone :: Text
  , email :: Text
  , sex :: Sex
  , dobDay :: Int
  , dobMonth :: Int
  , dobYear :: Int
  , mpdga :: Maybe Int
  , membership :: Membership
  , license :: License
  , disc :: Bool
  , magazine :: Bool
  } deriving (Show)

countSum :: Member -> Int
countSum Member {..} =
  (msSum membership) + (licenseSum membership license) + (discSum disc) + (magazineSum magazine)
  where
    msSum :: Membership -> Int
    msSum Adult = 15
    msSum Junior = 5
    licenseSum :: Membership -> License -> Int
    licenseSum _ NoLicense = 0
    licenseSum Adult A = 55
    licenseSum Adult B = 20
    licenseSum Junior A = 30
    licenseSum Junior B = 10
    discSum :: Bool -> Int
    discSum True = 10
    discSum False = 0
    magazineSum :: Bool -> Int
    magazineSum True = 30
    magazineSum False = 0

dayOptions :: [(Text, Int)]
dayOptions = map (\d -> (tshow d, d)) [1..31]

monthOptions :: [(Text, Int)]
monthOptions = map (\m -> (tshow m, m)) [1..12]

yearOptions :: [(Text, Int)]
yearOptions = map (\y -> (tshow y, y)) [1915..2015]

sexOptions :: [(Text, Sex)]
sexOptions = [("male", Male), ("female", Female)]

membershipOptions :: [(Text, Membership)]
membershipOptions = [("adult", Adult), ("junior", Junior)]

licenseOptions :: [(Text, License)]
licenseOptions =
  [ ("no-license", NoLicense)
  , ("adult", A)
  , ("junior", A)
  , ("adult", B)
  , ("junior", B)
  ]

data Sex = Male | Female
  deriving (Show, Eq)

data Membership = Adult | Junior
  deriving (Show, Eq)

data License = NoLicense | A | B
  deriving (Show, Eq)

-- bot checking
checkCheck :: Field Handler Text -> Field Handler Text
checkCheck = checkBool (\v -> v == "5" || toLower v == "viisi") MsgCheckError