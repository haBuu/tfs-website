module Model.Event where

import Import

import qualified Data.Text as T

getEvents :: Day -> Day -> DB [Entity Event]
getEvents start end =
  selectList
  (     [EventStartDate >=. start, EventStartDate <=. end]
    ||. [EventEndDate >=. Just start, EventEndDate <=. Just end]
  )
  [Desc EventStartDate]

getEventsDay :: Day -> DB [Entity Event]
getEventsDay day =
  selectList
  (     [EventStartDate ==. day]
    ||. [EventStartDate <. day, EventEndDate >=. Just day]
  )
  [Desc EventStartDate]