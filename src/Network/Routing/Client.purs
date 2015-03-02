module Network.Routing.Client
  ( EffRouting()

  , RoutingM(), Routing()
  , runRouter
  , unsafeGlobalRoute

  , Callback()
  , SetRoute()
  , setRoute

  , Z(), S(), Path(), Pathes()
  , Pathes0(), Pathes1(), Pathes2(), Pathes3()
  , useHistoryAPI, notFound

  , empty, exact, any, regex
  , (-/), (+/)
  , param

  , routes0, routes1, routes2, routes3
  , route0, route1, route2, route3
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Unsafe

import Data.String(joinWith)
import Data.Array()
import Data.Function
import Data.Maybe

import Network.Routing.Client.Foreign

foreign import data Routing :: !
foreign import data Dummy   :: *
foreign import data Router  :: *

-- routing Monad
type RoutingState eff =
  { variableIndex  :: Number
  , routerInstance :: Router
  , historyAPI     :: Boolean
  , notFound       :: Maybe (Callback eff Unit)
  }

type EffRouting eff = Eff (routing :: Routing | eff)

newtype RoutingM eff a = RoutingM
  (RoutingState eff -> Eff eff {a :: a, s :: RoutingState eff})

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

-- routing Monad methods
getState :: forall eff. RoutingM eff (RoutingState eff)
getState = RoutingM $ \s -> return {a: s, s: s}

modifyState :: forall eff. (RoutingState eff -> RoutingState eff) -> RoutingM eff Unit
modifyState f = RoutingM $ \s -> return {a: unit, s: f s}

useHistoryAPI :: RoutingM _ Unit
useHistoryAPI = modifyState (\s -> s{historyAPI = true})

notFound :: forall eff. Callback eff Unit -> RoutingM eff Unit
notFound m = modifyState (\s -> s{notFound = Just m})

succIndex :: forall eff. RoutingM eff Unit
succIndex = modifyState (\s -> s {variableIndex = s.variableIndex + 1})

liftRoutingM :: forall eff a. Eff eff a -> RoutingM eff a
liftRoutingM m = RoutingM $ \s -> m >>= \a -> return {a: a, s: s}

foreign import newRouter """
function newRouter(director){
  return function NewRouterEff(){
    return director();
  }
}""" :: forall eff. Director -> Eff eff Router

foreign import initRouter """
function initRouter(d){
  return function InitRouterEff(){
    d.init('/');
    return {};
  }
}""" :: forall eff. Router -> Eff eff Unit

foreign import configureImpl """
function configureImpl(r, opts){
  return function ConfigureEff(){
    r.configure({'html5history': opts.historyAPI, 'notfound': opts.notFound});
    return {};
  }
}""" :: forall eff opts. Fn2 Router {|opts} (Eff eff Unit)

type SetRoute  eff = String -> EffRouting eff Unit
type SetRoute_ eff = String -> Eff eff Unit

foreign import globalize """
function globalize(m){
  return m();
}""" :: forall eff a. Eff eff a -> a

unsafeGlobalRoute :: forall eff. RoutingM (routing :: Routing | eff) _ -> SetRoute eff
unsafeGlobalRoute m = globalize (runRouter m)

runRouter :: forall eff. RoutingM (routing :: Routing | eff) _ -> EffRouting eff (SetRoute eff)
runRouter (RoutingM m) = do
  r <- newRouter director
  o <- m {variableIndex: 0, routerInstance: r, historyAPI: false, notFound: Nothing}
  let s = o.s
  case s.notFound of
       Nothing -> runFn2 configureImpl r {historyAPI: s.historyAPI}
       Just nf -> runFn2 configureImpl r {historyAPI: s.historyAPI, notFound: runCallback nf (\s -> runFn2 setRouteImpl r s)}
  initRouter r
  return $ \s -> runFn2 setRouteImpl r s

-- Callback Monad
newtype Callback eff a = Callback (SetRoute_ eff -> Eff eff a)

runCallback :: forall eff a. Callback eff a -> SetRoute_ eff -> Eff eff a
runCallback (Callback m) = m

instance functorCallback :: Functor (Callback eff) where
  (<$>) f (Callback m) = Callback $ \s -> m s >>= \n -> return (f n)

instance applyCallback :: Apply (Callback eff) where
  (<*>) (Callback mf) (Callback ma) = Callback $ \s -> do
    f <- mf s
    a <- ma s
    return (f a)

instance applicativeCallback :: Applicative (Callback eff) where
  pure a = Callback $ \_ -> return a

instance bindCallback :: Bind (Callback eff) where
  (>>=) (Callback m) k = Callback $ \s -> do
    a <- m s
    Callback r <- return $ k a
    r s

instance monadCallback :: Monad (Callback eff)

instance monadEffCallback :: MonadEff eff (Callback eff) where
  liftEff m = Callback $ \_ -> m

foreign import setRouteImpl """
function setRouteImpl(d, p){
  return function SetRouteEff(){
    d.setRoute(p);
    return {};
  }
}""" :: forall eff. Fn2 Router String (Eff eff Unit)

setRoute :: String -> Callback _ Unit
setRoute route = Callback $ \set -> set route

-- | path piecies
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
}""" :: forall eff. Fn3 Router String String (Eff eff Unit)

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
}""" :: forall eff path fun. Fn3 Router path fun (Eff eff Unit)

route p f = do
  s <- getState
  liftRoutingM $ runFn3 routeImpl s.routerInstance (pathesToString p) f

foreign import wrap0 """
function wrap0(f){
  return function Wrap0(){return f();}
}""" :: forall eff z. Eff eff z -> Dummy

routes0 :: forall eff. Pathes0 -> [Eff eff _] -> RoutingM eff Unit
routes0 p f = route p (wrap0 <$> f)

route0 :: forall eff. Pathes0 -> Eff eff _ -> RoutingM eff Unit
route0 p f = routes0 p [f]

foreign import wrap1 """
function wrap1(f){
  return function Wrap1(a){
    var _this = this;
    var setRoute = function(route){
      return function(){
        _this.setRoute(route);
      }
    };
    return f(setRoute)(a)();
  }
}""" :: forall eff z. (SetRoute_ eff -> String -> Eff eff z) -> Dummy

routes1 :: forall eff. Pathes1 -> [String -> Callback eff Unit] -> RoutingM eff Unit
routes1 p f = route p ((\r -> wrap1 $ \set p1 -> runCallback (r p1) set) <$> f)

route1 :: forall eff. Pathes1 -> (String -> Callback eff Unit) -> RoutingM eff Unit
route1 p f = routes1 p [f]

foreign import wrap2 """
function wrap2(f){
  return function Wrap2(a,b){
    var _this = this;
    var setRoute = function(route){
      return function(){
        _this.setRoute(route);
      }
    };
    return f(setRoute)(a)(b)();
  }
}""" :: forall eff z. (SetRoute_ eff -> String -> String -> Eff eff z) -> Dummy

routes2 :: forall eff. Pathes2 -> [String -> String -> Callback eff Unit] -> RoutingM eff Unit
routes2 p f = route p ((\r -> wrap2 $ \set p1 p2 -> runCallback (r p1 p2) set) <$> f)

route2 :: forall eff. Pathes2 -> (String -> String -> Callback eff Unit) -> RoutingM eff Unit
route2 p f = routes2 p [f]

foreign import wrap3 """
function wrap3(f){
  return function Wrap3(a,b,c){
   var _this = this;
    var setRoute = function(route){
      return function(){
        _this.setRoute(route);
      }
    };
    return f(setRoute)(a)(b)(c)();
  }
}""" :: forall eff z. (SetRoute_ eff -> String -> String -> String -> Eff eff z) -> Dummy

routes3 :: forall eff. Pathes3 -> [String -> String -> String -> Callback eff Unit] -> RoutingM eff Unit
routes3 p f = route p ((\r -> wrap3 $ \set p1 p2 p3 -> runCallback (r p1 p2 p3) set) <$> f)

route3 :: forall eff. Pathes3 -> (String -> String -> String -> Callback eff Unit) -> RoutingM eff Unit
route3 p f = routes3 p [f]
