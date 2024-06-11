{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import

-- Define our data that will be used for creating the form.
data FileForm = FileForm
  { fileInfo :: FileInfo
  , fileDescription :: Text
  }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = renderWidget MsgHomepageTitle $ do
  $(widgetFile "wrap-image")
  $(widgetFile "history")
postHomeR :: Handler Html
postHomeR = do
  defaultLayout $ do
    setTitle "Welcome To Yesod!"
    $(widgetFile "history")
