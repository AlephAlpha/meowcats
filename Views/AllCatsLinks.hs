{-# LANGUAGE OverloadedStrings #-}

module Views.AllCatsLinks (allCatsLinks) where

import           Control.Monad
import qualified Data.Text.Lazy                as T
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

allCatsLinks :: [(String, T.Text)] -> Html
allCatsLinks allCats = html $ body $ forM_ allCats $ \(cat, catSrc) -> do
  a ! href (lazyTextValue catSrc) ! target "_blank" $ toHtml cat
  br
