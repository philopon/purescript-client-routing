# Module Documentation

## Module Network.Routing.Client

#### `Routing`

``` purescript
data Routing :: !
```


#### `EffRouting`

``` purescript
type EffRouting eff = Eff (routing :: Routing | eff)
```


#### `RoutingM`

``` purescript
newtype RoutingM eff a
```

Routing Monad

#### `functorRoutingM`

``` purescript
instance functorRoutingM :: Functor (RoutingM eff)
```


#### `applyRoutingM`

``` purescript
instance applyRoutingM :: Apply (RoutingM eff)
```


#### `applicativeRoutingM`

``` purescript
instance applicativeRoutingM :: Applicative (RoutingM eff)
```


#### `bindRoutingM`

``` purescript
instance bindRoutingM :: Bind (RoutingM eff)
```


#### `monadRoutingM`

``` purescript
instance monadRoutingM :: Monad (RoutingM eff)
```


#### `useHistoryAPI`

``` purescript
useHistoryAPI :: RoutingM _ Unit
```

use HTML5 history api instead of location.hash encode

#### `notFound`

``` purescript
notFound :: forall eff. Callback eff Unit -> RoutingM eff Unit
```

set handler for route not match

#### `SetRoute`

``` purescript
type SetRoute eff = String -> EffRouting eff Unit
```


#### `unsafeGlobalRoute`

``` purescript
unsafeGlobalRoute :: forall eff. RoutingM (routing :: Routing | eff) _ -> SetRoute eff
```


#### `unsafeGlobalRoute'`

``` purescript
unsafeGlobalRoute' :: forall eff. RoutingM (routing :: Routing | eff) _ -> { init :: EffRouting eff Unit, setRoute :: SetRoute eff }
```


#### `runRouter`

``` purescript
runRouter :: forall eff. RoutingM (routing :: Routing | eff) _ -> EffRouting eff (SetRoute eff)
```

run Router Monad

#### `runRouter'`

``` purescript
runRouter' :: forall eff. RoutingM (routing :: Routing | eff) _ -> EffRouting eff { init :: EffRouting eff Unit, setRoute :: SetRoute eff }
```

run Router Monad without initialize

#### `Callback`

``` purescript
newtype Callback eff a
```

Callback Monad

#### `functorCallback`

``` purescript
instance functorCallback :: Functor (Callback eff)
```


#### `applyCallback`

``` purescript
instance applyCallback :: Apply (Callback eff)
```


#### `applicativeCallback`

``` purescript
instance applicativeCallback :: Applicative (Callback eff)
```


#### `bindCallback`

``` purescript
instance bindCallback :: Bind (Callback eff)
```


#### `monadCallback`

``` purescript
instance monadCallback :: Monad (Callback eff)
```


#### `monadEffCallback`

``` purescript
instance monadEffCallback :: MonadEff eff (Callback eff)
```


#### `setRoute`

``` purescript
setRoute :: String -> Callback _ Unit
```

set route in Callback monad.
convenient to redirect.

#### `getSetRoute`

``` purescript
getSetRoute :: forall eff. Callback (routing :: Routing | eff) (SetRoute eff)
```


#### `Z`

``` purescript
data Z
```

#### `S`

``` purescript
data S n
```


#### `Path`

``` purescript
data Path n
```

Path data type

#### `showPath`

``` purescript
instance showPath :: Show (Path n)
```


#### `Pathes`

``` purescript
newtype Pathes n
```


#### `showPathes`

``` purescript
instance showPathes :: Show (Pathes n)
```


#### `Pathes0`

``` purescript
type Pathes0 = Pathes Z
```


#### `Pathes1`

``` purescript
type Pathes1 = Pathes (S Z)
```


#### `Pathes2`

``` purescript
type Pathes2 = Pathes (S (S Z))
```


#### `Pathes3`

``` purescript
type Pathes3 = Pathes (S (S (S Z)))
```


#### `empty`

``` purescript
empty :: Pathes0
```

empty path

#### `exact`

``` purescript
exact :: String -> Path Z
```

add exact match for path piece

#### `any`

``` purescript
any :: Path (S Z)
```

get any parameter from path piece

#### `regex`

``` purescript
regex :: String -> Path (S Z)
```

get regexed parameter from path piece

#### `(-/)`

``` purescript
(-/) :: forall n. Path Z -> Pathes n -> Pathes n
```

add path piece without parameter to pathes

#### `(+/)`

``` purescript
(+/) :: forall n. Path (S Z) -> Pathes n -> Pathes (S n)
```

add path piece with parameter to pathes

#### `param`

``` purescript
param :: forall eff n. Path n -> RoutingM eff (Path n)
```

create parameter in pathes

#### `routes0`

``` purescript
routes0 :: forall eff. Pathes0 -> [Eff eff _] -> RoutingM eff Unit
```

add routes which have no parameter

#### `route0`

``` purescript
route0 :: forall eff. Pathes0 -> Eff eff _ -> RoutingM eff Unit
```

add route which have no parameter

#### `routes1`

``` purescript
routes1 :: forall eff. Pathes1 -> [String -> Callback eff Unit] -> RoutingM eff Unit
```

add routes which have 1 parameter

#### `route1`

``` purescript
route1 :: forall eff. Pathes1 -> (String -> Callback eff Unit) -> RoutingM eff Unit
```

add route which have 1 parameter

#### `routes2`

``` purescript
routes2 :: forall eff. Pathes2 -> [String -> String -> Callback eff Unit] -> RoutingM eff Unit
```

add routes which have 2 parameters

#### `route2`

``` purescript
route2 :: forall eff. Pathes2 -> (String -> String -> Callback eff Unit) -> RoutingM eff Unit
```

add route which have 2 parameters

#### `routes3`

``` purescript
routes3 :: forall eff. Pathes3 -> [String -> String -> String -> Callback eff Unit] -> RoutingM eff Unit
```

add routes which have 3 parameters

#### `route3`

``` purescript
route3 :: forall eff. Pathes3 -> (String -> String -> String -> Callback eff Unit) -> RoutingM eff Unit
```

add route which have 3 parameters



