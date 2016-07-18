
module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception
import Control.Monad.Aff-- (launchAff)
import Data.Either (Either(..))
import Data.Maybe
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax-- (affjax, defaultRequest)
import Control.Monad.Eff
import Partial.Unsafe
import Control.Monad.Eff.Console

import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)

import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element(), ElementId(..), documentToNonElementParentNode)

import React
import ReactDOM (render)

import React.DOM as D
import React.DOM.Props as P

saneFromJust :: forall a. Maybe a -> a
saneFromJust (Just a) = a 
saneFromJust Nothing = unsafeCrashWith "fromJust"

main ::  Eff (ajax :: AJAX    
                                    , err :: EXCEPTION
                                    , dom :: DOM ) Unit
main = void $ launchAff $ do
  --liftEff $ (log "serthsrethrsFirst") -- res.response)
  res <- affjax $ defaultRequest { url = "/hello", method = Left GET }
  --liftEff $ (log "serthsrethrs") -- res.response)
  liftEff $ void (elm' >>= render (ui res) )
 where
 ui res = D.div [] [ D.text res.response]
 elm' = do
  win <- window
  doc <- document win
  elm <- getElementById (ElementId "example") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  pure $ saneFromJust (toMaybe elm)
{-
s::forall d. d -> d -> d
s a b = b
- }
- }
{-module Main where

import Prelude
import Partial
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Con-l.Monad.Eff.Console
--import Partial.Unsafe

import Data.Maybe 
import Data.Either 
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)

import DOM.Node.NonElementParentNode (getElementById)
--import DOM.Node.Types (Element(), ElementId(..), documentToNonElementParentNode)
import DOM.Node.Types 
import DOM.HTML.Types
import React
import ReactDOM (render, unmountComponentAtNode)

import  React.DOM as D
import  React.DOM.Props as P
import Network.HTTP.Affjax 

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Data.HTTP.Method

main ::  Eff ({-ajax :: AJAX    
                                    , err :: EXCEPTION
                                    ,- } dom :: DOM ) Unit
{-main = void $ launchAff $ do
  res <- pure {response:"ergagraerag"} -- affjax $ defaultRequest { url = "localhost:3000/api", method = Left GET }
  liftEff $ (elm' >>= render (ui res.response))
  - }
main = do 
    e <- elm'
    response <- pure "hello"
    render ( ui response) e
    pure unit
  
ui a = D.div' [ D.text a] {-createFactory hello { name: "World" }
          , createFactory counter unit
          , createElement container unit
                          [ D.p [ P.key "1" ] [ D.text  "This is line one" ]
                          , D.p [ P.key "2" ] [ D.text "This is line two" ]
                          ]
          - }
elm' ::  forall effelm.  Eff ( dom :: DOM | effelm) (Element)
elm' = do
    win <- window
    doc <- document win
    elm <- getElementById (ElementId "example") (documentToNonElementParentNode (htmlDocumentToDocument doc))
    pure $ case (toMaybe (elm)) of
            Just x -> x
            Nothing -> r 4

--r unit
r a = r a
{-    Contact GitHub API Training Shop Blog About 

    © 2016 GitHub, Inc. Terms Privacy Security Status Help 
-}