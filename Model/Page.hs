module Model.Page where

import Import

topLevelFilter :: Page -> Bool
topLevelFilter page = (pageTopLevel page) && (not $ pageObsolete page)

otherFilter :: Page -> Bool
otherFilter page = (not $ pageTopLevel page) && (not $ pageObsolete page)

obsoleteFilter :: Page -> Bool
obsoleteFilter page = pageObsolete page