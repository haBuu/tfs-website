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

getPostsLimit :: Int -> DB [(Entity Post, Entity User)]
getPostsLimit postLimit = E.select $
  E.from $ \(post, user) -> do
    E.where_ $ post ^. PostUserCreated E.==. user ^. UserId
    E.orderBy [E.desc (post ^. PostCreated)]
    E.limit $ fromIntegral postLimit
    return (post, user)