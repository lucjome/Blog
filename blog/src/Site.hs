{-# LANGUAGE OverloadedStrings, BlockArguments #-}

module Site (
    siteMain
) where

import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Network.HTTP.Types as HTTP
import qualified Web.Scotty as S
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Lucid as H
import qualified Data.List (intercalate, drop, take, reverse)
import Data.String (String)
import Web.Scotty
import qualified Network.Wai.Middleware.Static as Static

-- Types
data Post = Post { contentP :: TL.Text, titleP :: TL.Text, pdfP :: FilePath } 
data MyState = MyState { sId :: Int, sPosts :: Posts}

-- Synonyms
type Posts = M.Map Integer Post
type Html = H.Html ()
type WebState = STM.TVar MyState
type Ambig a = H.HtmlT a ()

-- HTML types

-- HTML definitions

-- TODO: write without do-notation

homePage :: Posts -> Html
homePage ps = do
    H.doctypehtml_ $ do 
        H.head_ $ do
            H.meta_ [ H.charset_ "utf-8" ]
            H.title_ (H.toHtml (titleP p))
            H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css" ]
        H.body_ $ do
            H.div_ [ H.class_ "main" ] $ do
                H.h1_ [ H.class_ "logo" ] $
                    H.a_ [ H.href_ "/" ] "luca$hevik"
                H.toHtml $ do
                 contentP <$> something -- fixa github!!

siteMain :: IO ()
siteMain = mkRandomIO >>= 
    \posts  -> STM.newTVarIO MyState{sId = 1, sPosts = posts} >>=
                         \myState -> S.scotty 3000 $ webApp myState


webApp :: WebState -> S.ScottyM ()
webApp myState = 
    S.get "/" $ liftIO (sPosts <$> STM.readTVarIO myState) >>= \posts ->
        S.html $ H.renderText $ homePage posts

-- A random post
mkRandomIO :: IO Posts
mkRandomIO = C.getCurrentTime >>= 
    \time -> pure $ M.singleton 0 $ Post {
         contentP = "Content"
        ,titleP = "Title" 
        ,pdfP = "empty"}
