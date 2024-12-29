{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = renderWidget MsgHomepageTitle $ do
  $(widgetFile "wrap-image")
  $(widgetFile "homepage")

tabList :: [TabItem]
tabList =
  [ TabItem
      { tabItemLabel = MsgHistoryTab
      , tabItemId = "1"
      , tabItemContent = $(widgetFile "history")
      , isChecked = False
      }
  , TabItem
      { tabItemLabel = MsgRandomTab1
      , tabItemId = "2"
      , tabItemContent = $(widgetFile "placeholder")
      , isChecked = True
      }
  , TabItem
      { tabItemLabel = MsgRandomTab2
      , tabItemId = "3"
      , tabItemContent = $(widgetFile "placeholder")
      , isChecked = False
      }
  ]
