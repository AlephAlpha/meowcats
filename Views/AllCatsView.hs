{-# LANGUAGE OverloadedStrings #-}

module Views.AllCatsView (allCatsView) where

import           Control.Monad
import qualified Data.Text.Lazy                as T
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

allCatsView :: [(String, T.Text)] -> Html
allCatsView allCats = html $ body $ forM_ allCats $ \(_, catSrc) ->
  img ! src (lazyTextValue catSrc) ! alt "cat gifs!"
