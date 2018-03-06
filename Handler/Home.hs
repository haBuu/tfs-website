module Handler.Home where

import Import

import Data.Time.LocalTime

import Helpers
import Members
import Model.Post

-- how many posts get displayed in one page
postsPerPage :: Int
postsPerPage = 5

-- convenience handler for the user
-- meaning "/" will be the same as "/1"
getHomeR :: Handler Html
getHomeR = getHomePageR 1

getHomePageR :: Int -> Handler Html
getHomePageR page = do
  (posts, postCount) <- runDB $ do
    p <- getPosts page postsPerPage
    c <- count ([] :: [Filter Post])
    return (p, c)
  let pages = ceiling $ (fromIntegral postCount / fromIntegral postsPerPage :: Double)
  tz <- liftIO getCurrentTimeZone
  bagtag <- bagtagWidget
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "home")

pageLinks :: Int -> Int -> [Int]
pageLinks page pages
  -- page can't be zero or negative
  | page < 1 = []
  -- trivial case when 5 or fewer pages
  | pages <= 5 = [1..pages]
  -- first tree pages
  | page <= 3 = [1,2,3,4,pages]
  -- last tree pages
  | pages - page < 3 = [1, pages - 3, pages - 2, pages - 1, pages]
  -- normal case
  | otherwise = [1, page - 1, page, page + 1, pages]