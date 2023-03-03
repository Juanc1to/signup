{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.EventSeriesList where

import Import

getEventSeriesListR :: Handler Html
getEventSeriesListR = do
  series_descriptions <- runDB $ selectList ([] :: [Filter EventSeries]) []
  defaultLayout $ case Prelude.length series_descriptions of
    0 -> toWidget [hamlet|<p>No event series descriptions available yet!|]
    --n -> toWidget [hamlet|<p>Found #{n} event series descriptions!|]
    _ -> $(widgetFile "browse")
