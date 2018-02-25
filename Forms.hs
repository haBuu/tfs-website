{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Forms where

import Import

import qualified Data.Text as T

import Yesod.Form.Bootstrap3

-- form handler with default action for FormFailure and FormMissing
formHandler :: FormResult a -> (a -> Handler ()) -> Handler ()
formHandler result f =
  case result of
    FormSuccess res -> f res
    FormFailure err -> setMessageI $ MsgFormFailure $ T.concat err
    FormMissing -> setMessageI MsgFormMissing

-- default submit button
submitButton :: a -> BootstrapSubmit a
submitButton msg = BootstrapSubmit msg "btn btn-light btn-block btn-lg" []

withRows :: Text -> FieldSettings site -> FieldSettings site
withRows n fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("rows", n) : fsAttrs fs