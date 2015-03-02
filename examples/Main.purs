module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Network.Routing.Client
import Data.Function
import Debug.Trace

foreign import onDOMConentLoaded """
function onDOMConentLoaded(f){
  return function Eff(){
    document.addEventListener("DOMContentLoaded", function(){f()});
  }
}""" :: forall eff a. Eff eff a -> Eff eff Unit

foreign import attachOnClickById """
function attachOnClickById(id, fn) {
  return function Eff(){
    document.getElementById(id).addEventListener('click', function(){
      fn();
    });
  }
}""" :: forall eff a. Fn2 String (Eff eff a) (Eff eff Unit)

main = do
  set <- runRouter $ do

    -- variable in route
    api <- param $ exact "api"
    num <- param $ regex "[0-9]+"

    -- no parameter route
    route0 empty $ liftEff $ trace "root"
    route0 (api -/ empty) $ liftEff $ trace "api"

    -- parametered route
    route1 (api -/ num +/ empty) $ \n -> do
      liftEff $ trace $ "api number: " ++ n
      if n == "1"
         then do
           liftEff $ trace "redirect to /"
           setRoute "/"
         else return unit

    route2 (api -/ exact "show" -/ num +/ any +/ empty) $ \i a -> do
      liftEff $ trace $ "api show: " ++ i ++ ", " ++ a

    -- action when not found
    notFound $ do
      liftEff $ trace "not found"
      setRoute "/"

  -- set route
  onDOMConentLoaded $ do
    runFn2 attachOnClickById "root" $ set "/"
    runFn2 attachOnClickById "api" $ set "/api"
    runFn2 attachOnClickById "api-1" $ set "/api/1"
    runFn2 attachOnClickById "api-12" $ set "/api/12"
    runFn2 attachOnClickById "api-bad" $ set "/api/bad"
    runFn2 attachOnClickById "api-show" $ set "/api/show/12/cat"
