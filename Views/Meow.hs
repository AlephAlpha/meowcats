{-# LANGUAGE OverloadedStrings #-}

module Views.Meow (meow) where

import qualified Data.Text.Lazy              as T
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

meow :: T.Text -> Html
meow cat = html $ body $ img ! src (lazyTextValue cat)
