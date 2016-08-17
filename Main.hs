{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.Text.Lazy                as T
import           System.Directory
import           Test.QuickCheck.Gen
import           Text.Blaze.Html.Renderer.Text
import           Views.AllCatsLinks
import           Views.AllCatsView
import           Views.Meow
import           Web.Scotty

type Cat = String
type Url = T.Text

port :: Int
port = 3000

cdnUrl :: Url
cdnUrl = "http://localhost:3000"

catUrl :: Cat -> Url
catUrl cat = T.concat [cdnUrl, "/cats/", T.pack cat]

catDir :: IO FilePath
catDir = makeAbsolute "cats/"

catPath :: Cat -> IO FilePath
catPath cat = (++ cat) <$> catDir

catExists :: Cat -> IO Bool
catExists cat = doesFileExist =<< catPath cat

getAllCats :: IO [Cat]
getAllCats = drop 2 <$> (getDirectoryContents =<< catDir)

getRandomCat :: IO Cat
getRandomCat = generate . elements =<< getAllCats

addCatHeaders :: ActionM ()
addCatHeaders = addHeader "Access-Control-Allow-Origin" "*"

sendCat :: Cat -> ActionM ()
sendCat cat = do
  setHeader "Content-Type" "image/gif"
  file =<< liftAndCatchIO (catPath cat)

meowCats :: ScottyM ()
meowCats = do
  get "/meow" $ do
    catSrc <- catUrl <$> liftAndCatchIO getRandomCat
    html $ renderHtml $ meow catSrc
  get "/all/:allParam" $ do
    allCats <- map (ap (,) catUrl) <$> liftAndCatchIO getAllCats
    allParam <- param "allParam"
    case (allParam :: T.Text) of
      "show" -> html $ renderHtml $ allCatsView allCats
      "count" -> do
        let catCount = T.pack . show $ length allCats
        text $ T.concat ["There are ", catCount, " total cat gifs."]
      _ -> html $ renderHtml $ allCatsLinks allCats
  get "/all" $ redirect "/all/"
  get "/cats/:cat" $ do
    cat <- param "cat"
    catFound <- liftAndCatchIO $ catExists cat
    if catFound then sendCat cat else redirect "/"
  get "/random" $ do
    addCatHeaders
    text . catUrl =<< liftAndCatchIO getRandomCat
  get "/" $ do
    addCatHeaders
    cat <- liftAndCatchIO getRandomCat
    addHeader "X-Cat-Link" $ catUrl cat
    sendCat cat
  notFound $ redirect "/"

main :: IO ()
main = scotty port meowCats
