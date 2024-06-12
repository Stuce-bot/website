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
      , tabItemId = "history-tab"
      , tabItemContent = $(widgetFile "history")
      , isChecked = False
      }
  , TabItem
      { tabItemLabel = MsgRandomTab1
      , tabItemId = "tab2"
      , tabItemContent = $(widgetFile "placeholder")
      , isChecked = True
      }
  , TabItem
      { tabItemLabel = MsgRandomTab2
      , tabItemId = "tab3"
      , tabItemContent = $(widgetFile "placeholder")
      , isChecked = False
      }
  ]
