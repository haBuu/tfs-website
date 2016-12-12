module Members
( membersWidget
)
where

import Import hiding (for)

import Helpers

membersWidget :: Handler Widget
membersWidget = do
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