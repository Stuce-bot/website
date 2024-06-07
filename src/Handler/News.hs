{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.News where

import Import
import Yesod.Form.Nic (nicHtmlField)

entryForm :: Form Entry
entryForm =
  renderDivs $
    Entry
      <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
      <*> lift (liftIO getCurrentTime)
      <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing

getNewsR :: Handler Html
getNewsR = do
  muser <- maybeAuth
  entries <- runDB $ selectList [] [Desc EntryPosted]
  (entryWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    setTitleI MsgNewsArchiveTitle
    $(widgetFile "news")

postNewsR :: Handler Html
postNewsR = do
  ((res, entryWidget), enctype) <- runFormPost entryForm
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessageI $ MsgEntryCreated $ entryTitle entry
      redirect $ NewsEntryR entryId
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectEntry
      [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=_{MsgNewEntry}>
|]

getNewsEntryR :: EntryId -> Handler Html
getNewsEntryR entryId = do
  entry <- runDB $ do
    get404 entryId
  defaultLayout $ do
    setTitleI $ MsgEntryTitle $ entryTitle entry
    [whamlet|
<h1>#{entryTitle entry}
<article>#{entryContent entry}
|]
