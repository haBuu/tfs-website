{-# LANGUAGE DeriveGeneric #-}

module Members
( membersWidget
, bagtagWidget
)
where

import Import hiding (for, get, responseBody)

import GHC.Generics (Generic)
import Network.Wreq
import Control.Lens
-- import Control.Exception
import Data.Aeson (FromJSON)
import Data.Aeson.Types (typeMismatch)

import Helpers

data Player = Player
  { number :: String
  , name :: Maybe String
  } deriving (Show, Generic)

data Players = Players
  { players :: [Player]
  , errors :: [String]
  } deriving (Show, Generic)

instance FromJSON Players where
  parseJSON (Object v) = Players
    <$> v .: "players"
    <*> v .: "Errors"
  parseJSON invalid = typeMismatch "Players" invalid

instance FromJSON Player where
  parseJSON (Object v) = Player
    <$> v .: "Number"
    <*> v .: "Name"
  parseJSON invalid = typeMismatch "Player" invalid

metrixAPI :: String
metrixAPI = "https://discgolfmetrix.com/api.php?content=bagtag_list&id=6"

getPlayerData :: IO [Player]
getPlayerData = do
  response <- asJSON =<< get metrixAPI
  return $ players $ response ^. responseBody

getBagtagPlayers :: IO [Player]
getBagtagPlayers = do
  catch getPlayerData handler
  where
    handler :: JSONError -> IO [Player]
    handler _ = return []

bagtagWidget :: Handler Widget
bagtagWidget = do
  players <- liftIO getBagtagPlayers
  return $ bagtagWidget' players

bagtagWidget' :: [Player] -> Widget
bagtagWidget' players = [whamlet|
  <div .card>
    <h3 .card-header>
      Bagtag
    <div .card-block>
      <ol>
        $forall (Player _ mp) <- players
          <li>
            $maybe p <- mp
              #{p}
            $nothing
              -
|]

membersWidget :: Handler Widget
membersWidget = do
  players <- liftIO getBagtagPlayers
  liftIO $ print players
  members <- runDB $ selectList
    [ ClubMemberName !=. Nothing
    , ClubMemberRating !=. Nothing
    ]
    [Asc ClubMemberRating, LimitTo 10]
  return $ membersWidget' $ zip [1..] members

membersWidget' :: [(Int, Entity ClubMember)] -> Widget
membersWidget' members = [whamlet|
  <div .mt-3 #members .mx-auto>
    <table .table .table-hover .table-sm .mx-auto>
      <thead .thead-default>
        <tr>
          <th colspan=3>Rating top 10
      <tbody>
        $forall (i, Entity _ member) <- members
          <tr>
            <td>
              #{show i}
            <td #name>
              $maybe name <- clubMemberName member
                #{name}
              $nothing
                N/A
            <td #rating>
              $maybe rating <- clubMemberRating member
                #{show rating}
              $nothing
                N/A
|]