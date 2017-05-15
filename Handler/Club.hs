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

pdgaDisc = 15
pdgaMagazine = 35
pdgaLicenseAdult = 50
pdgaLicenseJunior = 25
membershipAdult = 15
membershipJunior = 5

getClubR :: Handler Html
getClubR = do
  (year, _, _) <- liftM (toGregorian . utctDay) $ liftIO getCurrentTime
  let formDays, formMonths, formYears :: [Int]
      formDays = [1..31]
      formMonths = [1..12]
      formYears = [1915..fromIntegral year]
      juniorLimit = year - 18
  calendar <- calendarWidget
  bagtag <- bagtagWidget
  content <- runDB $ getPageMarkdown Club
  defaultLayout $ do
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "club")

postClubR :: Handler Html
postClubR = do
  (year, _, _) <- liftM (toGregorian . utctDay) $ liftIO getCurrentTime
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
    <*> ireq (selectFieldList $ yearOptions year) "year"
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
            #{showLicense license}
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

showLicense :: License -> String
showLicense NoLicence = "Ei kilpailulisenssiä"
showLicense PDGA_Adult = "Kilpailulisenssi, aikuinen"
showLicense PDGA_Junior = "Kilpailulisenssi, juniori"

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
  (msSum membership) + (licenseSum license) + (discSum disc) + (magazineSum magazine)
  where
    msSum :: Membership -> Int
    msSum Adult = membershipAdult
    msSum Junior = membershipJunior
    licenseSum :: License -> Int
    licenseSum NoLicence = 0
    licenseSum PDGA_Adult = pdgaLicenseAdult
    licenseSum PDGA_Junior = pdgaLicenseJunior
    discSum :: Bool -> Int
    discSum True = pdgaDisc
    discSum False = 0
    magazineSum :: Bool -> Int
    magazineSum True = pdgaMagazine
    magazineSum False = 0

dayOptions :: [(Text, Int)]
dayOptions = map (\d -> (tshow d, d)) [1..31]

monthOptions :: [(Text, Int)]
monthOptions = map (\m -> (tshow m, m)) [1..12]

yearOptions :: Integer -> [(Text, Int)]
yearOptions year = map (\y -> (tshow y, y)) [1915..fromIntegral year]

sexOptions :: [(Text, Sex)]
sexOptions = [("male", Male), ("female", Female)]

membershipOptions :: [(Text, Membership)]
membershipOptions = [("adult", Adult), ("junior", Junior)]

licenseOptions :: [(Text, License)]
licenseOptions =
  [ ("no-licence", NoLicence)
  , ("adult", PDGA_Adult)
  , ("junior", PDGA_Junior)
  ]

data Sex = Male | Female
  deriving (Show, Eq)

data Membership = Adult | Junior
  deriving (Show, Eq)

data License = NoLicence | PDGA_Adult | PDGA_Junior
  deriving (Show, Eq)

-- bot checking
checkCheck :: Field Handler Text -> Field Handler Text
checkCheck = checkBool (\v -> v == "5" || toLower v == "viisi") MsgCheckError