module Model.Page where

import Prelude
import Database.Persist.TH

data Page
  = Club
  | Companies
  | Courses
  | Competitions
  | Contact
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Page"