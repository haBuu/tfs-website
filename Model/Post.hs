{-# LANGUAGE ScopedTypeVariables #-}

module Model.Post where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

getPosts :: Int -> Int -> DB [Entity Post]
getPosts page postsPerPage
  | page > 0 && postsPerPage > 0 = selectList
    []
    [ Desc PostCreated
    , LimitTo postsPerPage
    , OffsetBy $ (page - 1) * postsPerPage
    ]
  | otherwise = return []

posts :: Int -> DB [(Entity Post, Entity User)]
posts postLimit = E.select $
  E.from $ \(post, user) -> do
    E.where_ $ post ^. PostUserCreated E.==. user ^. UserId
    E.orderBy [E.desc (post ^. PostCreated)]
    E.limit $ fromIntegral postLimit
    return (post, user)

mkPostFromEvent :: Event -> Post
mkPostFromEvent Event {..} =
  Post eventCreated eventUser Nothing Nothing eventTitle eventContent