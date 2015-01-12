module Main where

import Control.Monad.Eff
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
    route0 empty $ trace "root"
    route0 (api -/ empty) $ trace "api"

    -- parametered route
    route1 (api -/ num +/ empty) $ \n ->
      trace $ "api number: " ++ n

    route2 (api -/ exact "show" -/ num +/ any +/ empty) $ \i a -> do
      trace $ "api show: " ++ i ++ ", " ++ a

    -- action when not found
    notFound $ \set -> do
      trace "not found"
      set "/"

  -- set route
  onDOMConentLoaded $ do
    runFn2 attachOnClickById "root" $ set "/"
    runFn2 attachOnClickById "api" $ set "/api"
    runFn2 attachOnClickById "api-12" $ set "/api/12"
    runFn2 attachOnClickById "api-bad" $ set "/api/bad"
    runFn2 attachOnClickById "api-show" $ set "/api/show/12/cat"
