module PageMessage where

import Import

import Model.Page

pageMsg :: Page -> AppMessage
pageMsg page =
  case page of
    Club -> MsgClub
    Courses -> MsgCourses
    Competitions -> MsgCompetitions
    Contact -> MsgContact
    Companies -> MsgCompanies