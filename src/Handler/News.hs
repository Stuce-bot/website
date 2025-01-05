{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.News where

import Import

-- TODO: fix labels and use extra (learn what it does aswell ?)
postForm :: Maybe Entry -> Html -> MForm Handler (FormResult Entry, Widget)
postForm post extra = do
  (frRes, frView) <- mreq markdownField "contenu Fr" (entryContentFr <$> post)
  (deRes, deView) <- mreq markdownField "einhalt De" (entryContentDe <$> post)
  time <- pure <$> maybe (liftIO getCurrentTime) (return . entryTime) post
  (postedRes, postedView) <- mreq boolField "posted" $ Just $ maybe True entryPosted post

  let entryRes = Entry <$> frRes <*> deRes <*> time <*> postedRes

  let widget = do
        toWidget -- TODO: change border color + background color + i18n on submit
          [lucius|
                    .blogForm {
                        width: 3em;
                        display: flex;
                        gap: 20px;
                    }
                    .blogForm textarea {
                        width: 40vw;
                        height: 60vh;
                        resize: none;
                        padding: 10px;
                        border-radius: 5px;
                        border: 2px solid #ccc;
                    }
                |]
        [whamlet|
                <div>
                    #{extra}
                    <div .blogForm>
                        <div>
                          ^{fvLabel frView}
                          ^{fvInput frView}
                        <div>
                          ^{fvLabel deView}
                          ^{fvInput deView}

                ^{fvLabel postedView}
                ^{fvInput postedView}
            |]
  return (entryRes, widget)

-- markdownToHtmlTrusted does in fact either return an error or html, we have to decide how to handle that here, I decided to show a message to the user
handlePandocErrorOrHtml :: (Show a) => Either a Html -> Html
handlePandocErrorOrHtml (Left err) = toHtml $ "Error: " ++ show err -- Handle the PanducError case  TODO : we can add a way to contact admin when this shows up
handlePandocErrorOrHtml (Right html) = html -- Return the Html when successful

-- WARNING: the use of markdownToHtmlTrusted means we can use html, for example for better image positioning or iframes (gmaps), but this also means we have to make sure only trusted admins can post
markdownToBlogPost :: Markdown -> Html
markdownToBlogPost entry = handlePandocErrorOrHtml $ markdownToHtmlTrusted entry

getNewsR :: Handler Html
getNewsR = do
  muser <- maybeAuth

  de <- fmap prefersGerman languages
  entries <- if isAdmin muser then runDB $ selectList [] [Desc EntryTime] else runDB $ selectList [EntryPosted ==. True] [Desc EntryTime] -- TODO: select only posted news if not authenticated as admin,
  renderWidget MsgNewsArchiveTitle $ do
    $(widgetFile "news")

getNewNewsEntryR :: Handler Html
getNewNewsEntryR = do
  ((_, entryWidget), enctype) <- runFormPost $ postForm Nothing
  defaultLayout $ do
    setTitleI $ MsgEntryTitle "NewPost"
    [whamlet|
      <form method=post enctype=#{enctype}>
        ^{entryWidget}
        <div>
            <input type=submit value=_{MsgNewEntry}>
  |]

postNewNewsEntryR :: Handler Html
postNewNewsEntryR = do
  -- ((res, entryWidget), enctype) <- runFormPost $ entryForm Nothing
  ((res, entryWidget), enctype) <- runFormPost $ postForm Nothing
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessageI $ MsgEntryCreated $ pack $ show entryId
      redirect $ EditNewsEntryR entryId
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectEntry -- TODO: check how it could fail, and what better message could be given
      [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=_{MsgNewEntry}>
|]

getNewsEntryR :: EntryId -> Handler Html -- TODO: make a template for that, and remove duplicate code, fix title
getNewsEntryR entryId =
  do
    -- NOTE: we might find more elengant with more experience, but this db call is to avoid updating time on db update.
    entry <- runDB $ do
      get404 entryId
    de <- fmap prefersGerman languages
    defaultLayout $ do
      setTitleI $ MsgEntryTitle $ pack $ show entryId
      [whamlet|
  $if de
    <article>#{markdownToBlogPost $ entryContentDe entry}
  $else
    <article>#{markdownToBlogPost $ entryContentFr entry}
  |]

getEditNewsEntryR :: EntryId -> Handler Html -- TODO: make a template for that, and remove duplicate code
getEditNewsEntryR entryId =
  do
    -- NOTE: we might find more elengant with more experience, but this db call is to avoid updating time on db update.
    entry <- runDB $ do
      get404 entryId
    ((_, entryWidget), enctype) <- runFormPost $ postForm $ Just entry
    de <- fmap prefersGerman languages
    defaultLayout $ do
      setTitleI $ MsgEntryTitle $ pack $ show entryId
      [whamlet|
  $if de
    <article>#{markdownToBlogPost $ entryContentDe entry}
  $else
    <article>#{markdownToBlogPost $ entryContentFr entry}
  <form method=post enctype=#{enctype}>
      ^{entryWidget}
      <div>
        <input type=submit value=_{MsgNewEntry}>
  |]

postEditNewsEntryR :: EntryId -> Handler Html
postEditNewsEntryR entryId = do
  dbEntry <- runDB $ do
    get404 entryId
  ((res, entryWidget), enctype) <- runFormPost $ postForm $ Just dbEntry
  case res of
    FormSuccess formEntry -> do
      _ <- runDB $ replace entryId formEntry
      setMessageI $ MsgEntryCreated $ pack $ show entryId
      redirect $ EditNewsEntryR entryId
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectEntry
      [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=_{MsgNewEntry}>
|]

-- TODO: check how renderMessage works, and implement similar here to avoid code duplication instead of this ()

-- we still go trough the languages list because we need to check if we actually prefer german and not only if german is present
-- meaning we cant just check if german exists but if it comes before
prefersGerman :: [Text] -> Bool
prefersGerman [] = False -- default language
prefersGerman (x : xs)
  | x == "fr" = False
  | x == "fr_CH" = False
  | x == "de" = True
  | x == "de_CH" = True
  | otherwise = prefersGerman xs
