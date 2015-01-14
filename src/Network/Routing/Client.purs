module Network.Routing.Client
  ( EffRouting(), RoutingM(), Routing(), Router()
  , runRouter, getRouter
  , unsafeGlobalRoute
  , Z(), S(), Path(), Pathes()
  , Pathes0(), Pathes1(), Pathes2(), Pathes3()
  , SetRoute()
  , useHistoryAPI, notFound
  , empty, exact, any, regex
  , (-/), (+/)
  , param
  , routes0, routes1, routes2, routes3
  , route0, route1, route2, route3
  ) where

import Control.Monad.Eff

import Data.String(joinWith)
import Data.Array()
import Data.Function
import Data.Maybe

import Network.Routing.Client.Foreign

foreign import data Routing :: !
foreign import data Dummy   :: *
foreign import data Router  :: *

type RoutingState eff =
  { variableIndex  :: Number
  , routerInstance :: Router
  , historyAPI     :: Boolean
  , notFound       :: Maybe (SetRoute eff -> EffRouting eff Unit)
  }

type EffRouting eff = Eff (routing :: Routing | eff)

newtype RoutingM eff a = RoutingM
  (RoutingState eff -> EffRouting eff {a :: a, s :: RoutingState eff})

instance functorRoutingM :: Functor (RoutingM eff) where
  (<$>) f (RoutingM m) = RoutingM $ \s -> m s >>= \n -> return { a: f n.a, s: n.s }

instance applyRoutingM :: Apply (RoutingM eff) where
  (<*>) (RoutingM mf) (RoutingM ma) = RoutingM $ \s -> do
    f <- mf s
    a <- ma f.s
    return {a: f.a a.a, s: a.s}

instance applicativeRoutingM :: Applicative (RoutingM eff) where
  pure a = RoutingM $ \s -> return {a: a, s: s}

instance bindRoutingM :: Bind (RoutingM eff) where
  (>>=) (RoutingM m) k = RoutingM $ \s -> do
    a <- m s
    RoutingM r <- return $ k a.a
    r a.s

instance monadRoutingM :: Monad (RoutingM eff)

getState :: forall eff. RoutingM eff (RoutingState eff)
getState = RoutingM $ \s -> return {a: s, s: s}

getRouter :: forall eff. RoutingM eff Router
getRouter = getState >>= \s -> return s.routerInstance

modifyState :: forall eff. (RoutingState eff -> RoutingState eff) -> RoutingM eff Unit
modifyState f = RoutingM $ \s -> return {a: unit, s: f s}

useHistoryAPI :: RoutingM _ Unit
useHistoryAPI = modifyState (\s -> s{historyAPI = true})

notFound :: forall eff. (SetRoute eff -> EffRouting eff _) -> RoutingM eff Unit
notFound m = modifyState (\s -> s{notFound = Just $ \s -> void (m s)})

succIndex :: forall eff. RoutingM eff Unit
succIndex = modifyState (\s -> s {variableIndex = s.variableIndex + 1})

liftRoutingM :: forall eff a. EffRouting eff a -> RoutingM eff a
liftRoutingM m = RoutingM $ \s -> m >>= \a -> return {a: a, s: s}

foreign import newRouter """
function newRouter(director){
  return function NewRouterEff(){
    return director();
  }
}""" :: forall eff. Director -> EffRouting eff Router

foreign import initRouter """
function initRouter(d){
  return function InitRouterEff(){
    d.init('/');
    return {};
  }
}""" :: forall eff. Router -> EffRouting eff Unit

foreign import configureImpl """
function configureImpl(r, opts){
  return function ConfigureEff(){
    r.configure({'html5history': opts.historyAPI, 'notfound': opts.notFound});
    return {};
  }
}""" :: forall eff opts. Fn2 Router {|opts} (EffRouting eff Unit)

type SetRoute eff = String -> EffRouting eff Unit

foreign import globalize """
function globalize(m){
  return m();
}""" :: forall eff a. Eff eff a -> a

unsafeGlobalRoute :: forall eff. RoutingM eff _ -> SetRoute eff
unsafeGlobalRoute m = globalize (runRouter m)

runRouter :: forall eff. RoutingM eff _ -> EffRouting eff (SetRoute eff)
runRouter (RoutingM m) = do
  r <- newRouter director
  o <- m {variableIndex: 0, routerInstance: r, historyAPI: false, notFound: Nothing}
  let s = o.s
  case s.notFound of
       Nothing -> runFn2 configureImpl r {historyAPI: s.historyAPI}
       Just nf -> runFn2 configureImpl r {historyAPI: s.historyAPI, notFound: nf (\s -> runFn2 setRouteImpl r s)}
  initRouter r
  return $ \s -> runFn2 setRouteImpl r s

data Z
data S n

data Path n
  = Exact String
  | Regex String
  | Param String
  | Any

instance showPath :: Show (Path n) where
  show = pathToString

pathToString :: Path _ -> String
pathToString (Exact s) = s
pathToString (Regex r) = "(" ++ r ++ ")"
pathToString (Param p) = p
pathToString Any       = ":_"

newtype Pathes n = Pathes [Path n]

instance showPathes :: Show (Pathes n) where
  show = pathesToString

pathesToString :: Pathes _ -> String
pathesToString (Pathes ps) = "/" ++ joinWith "/" (pathToString <$> ps)

type Pathes0 = Pathes Z
type Pathes1 = Pathes (S Z)
type Pathes2 = Pathes (S (S Z))
type Pathes3 = Pathes (S (S (S Z)))

foreign import unsafeCoerce """
function unsafeCoerce(a){
  return a;
}""" :: forall a b. a -> b

empty :: Pathes0
empty = Pathes []

exact :: String -> Path Z
exact = Exact

any :: Path (S Z)
any = Any

regex :: String -> Path (S Z)
regex = Regex

(-/) :: forall n. Path Z -> Pathes n -> Pathes n
(-/) a (Pathes b) = Pathes (unsafeCoerce a:b)

(+/) :: forall n. Path (S Z) -> Pathes n -> Pathes (S n)
(+/) a (Pathes b) = Pathes (unsafeCoerce a:unsafeCoerce b)

infixr 5 -/
infixr 5 +/

foreign import paramImpl """
function paramImpl(d,n,m){
  return function ParamEff(){
    d.param(n,m);
    return {};
  }
}""" :: forall eff. Fn3 Router String String (EffRouting eff Unit)

param :: forall eff n. Path n -> RoutingM eff (Path n)
param v = do
  s <- getState
  let n = ":v" ++ show s.variableIndex
  liftRoutingM $ runFn3 paramImpl s.routerInstance n (pathToString v)
  succIndex
  return $ Param n

foreign import routeImpl """
function routeImpl(d,p,f){
  return function RouteEff(){
    d.on(p,f);
    return {};
  }
}""" :: forall eff path fun. Fn3 Router path fun (EffRouting eff Unit)

route p f = do
  s <- getState
  liftRoutingM $ runFn3 routeImpl s.routerInstance (pathesToString p) f

foreign import wrap0 """
function wrap0(f){
  return function Wrap0(){return f();}
}""" :: forall eff z. EffRouting eff z -> Dummy

foreign import setRouteImpl """
function setRouteImpl(d, p){
  return function SetRouteEff(){
    d.setRoute(p);
    return {};
  }
}""" :: forall eff. Fn2 Router String (EffRouting eff Unit)

routes0 :: forall eff. Pathes0 -> [EffRouting eff _] -> RoutingM eff Unit
routes0 p f = route p (wrap0 <$> f)

route0 :: forall eff. Pathes0 -> EffRouting eff _ -> RoutingM eff Unit
route0 p f = routes0 p [f]

foreign import wrap1 """
function wrap1(f){
  return function Wrap1(a){return f(a)();}
}""" :: forall eff z. (String -> EffRouting eff z) -> Dummy

routes1 :: forall eff. Pathes1 -> [String -> EffRouting eff _] -> RoutingM eff Unit
routes1 p f = route p (wrap1 <$> f)

route1 :: forall eff. Pathes1 -> (String -> EffRouting eff _) -> RoutingM eff Unit
route1 p f = routes1 p [f]

foreign import wrap2 """
function wrap2(f){
  return function Wrap2(a,b){return f(a,b)();}
}""" :: forall eff z. (Fn2 String String (EffRouting eff z)) -> Dummy

routes2 :: forall eff. Pathes2 -> [String -> String -> EffRouting eff _] -> RoutingM eff Unit
routes2 p f = route p ((\a -> wrap2 $ mkFn2 a) <$> f)

route2 :: forall eff. Pathes2 -> (String -> String -> EffRouting eff _) -> RoutingM eff Unit
route2 p f = routes2 p [f]

foreign import wrap3 """
function wrap3(f){
  return function Wrap3(a,b,c){return f(a,b,c)();}
}""" :: forall eff z. (Fn3 String String String (EffRouting eff z)) -> Dummy

routes3 :: forall eff. Pathes3 -> [String -> String -> String -> EffRouting eff _] -> RoutingM eff Unit
routes3 p f = route p ((\a -> wrap3 $ mkFn3 a) <$> f)

route3 :: forall eff. Pathes3 -> (String -> String -> String -> EffRouting eff _) -> RoutingM eff Unit
route3 p f = routes3 p [f]
