{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
  (_, user) <- requireAuthPair
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s User page"
    $(widgetFile "profile")

getProfileWidgetR :: Handler Html
getProfileWidgetR = do
  (_, user) <- requireAuthPair
  pageContent <-
    widgetToPageContent
      $(widgetFile "profile")
  withUrlRenderer $ pageBody pageContent
