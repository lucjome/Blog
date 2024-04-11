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
data Post = Post { contentP :: TL.Text, titleP :: TL.Text, minContentP:: TL.Text } 
data MyState = MyState { sId :: Int, sPosts :: Posts}

-- Synonyms
type Posts = M.Map Integer Post
type Html = H.Html ()
type WebState = STM.TVar MyState
type Ambig a = H.HtmlT a ()

-- HTML types
homePage :: Posts -> Html
postCan :: Int -> Post -> Html
-- HTML definitions

-- TODO: write without do-notation
-- TODO: div for about us, links, contact etc at the top 
-- TODO: display every post as a miniature + "Read more" link attached
homePage ps = do
    H.doctypehtml_ $ do 
        H.head_ $ do
            H.meta_ [ H.charset_ "utf-8" ]
            H.title_ (H.toHtml ("Home Page" :: TL.Text))
            H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css" ]
        H.body_ $ do
            H.div_ [ H.class_ "main" ] $ do
                H.h1_ [ H.class_ "logo" ] $
                    H.a_ [ H.href_ "/" ] "lupg - home"
                fmap H.toHtml TL.concat $ M.elems (
                    fmap minContentP ps) -- use H.with
        
-- every post should look like this
postCan pid ps = do
    H.doctypehtml_ $ do 
        H.head_ $ do
            H.meta_ [ H.charset_ "utf-8" ]
            H.title_ (H.toHtml (titleP ps <> " " <> TL.pack (show pid)))
            H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css" ]
        H.body_ $ do
            H.div_ [ H.class_ "main" ] $ do
                H.h1_ [ H.class_ "logo" ] $
                    H.a_ [ H.href_ "/" ] "back to homepage"


siteMain :: IO ()
siteMain = mkRandomIO >>= 
    \posts  -> STM.newTVarIO MyState{sId = 1, sPosts = posts} >>=
                         \myState -> S.scotty 3000 $ webApp myState


webApp :: WebState -> S.ScottyM ()
webApp myState = 
    S.get "/" $ liftIO (sPosts <$> STM.readTVarIO myState) >>= \posts ->
        S.html $ H.renderText $ homePage posts

-- take text (blog posts) from another directory into "Posts"
getPosts :: TL.Text -> Posts
getPosts = undefined

-- posts as a singular random post
mkRandomIO :: IO Posts
mkRandomIO = C.getCurrentTime >>= 
    \time -> pure $ M.singleton 0 $ Post {
         contentP = "This is my actual personal letter"
        ,titleP = "Title" 
        ,minContentP = "Hi this is a miniature version of my personal letter, I am writing a personal letter..."}
