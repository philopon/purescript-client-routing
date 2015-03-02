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


#### `notFound`

``` purescript
notFound :: forall eff. Callback eff Unit -> RoutingM eff Unit
```


#### `SetRoute`

``` purescript
type SetRoute eff = String -> EffRouting eff Unit
```


#### `unsafeGlobalRoute`

``` purescript
unsafeGlobalRoute :: forall eff. RoutingM (routing :: Routing | eff) _ -> SetRoute eff
```


#### `runRouter`

``` purescript
runRouter :: forall eff. RoutingM (routing :: Routing | eff) _ -> EffRouting eff (SetRoute eff)
```


#### `Callback`

``` purescript
newtype Callback eff a
```

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


#### `Z`

``` purescript
data Z
```

path piecies

#### `S`

``` purescript
data S n
```


#### `Path`

``` purescript
data Path n
```


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


#### `exact`

``` purescript
exact :: String -> Path Z
```


#### `any`

``` purescript
any :: Path (S Z)
```


#### `regex`

``` purescript
regex :: String -> Path (S Z)
```


#### `(-/)`

``` purescript
(-/) :: forall n. Path Z -> Pathes n -> Pathes n
```


#### `(+/)`

``` purescript
(+/) :: forall n. Path (S Z) -> Pathes n -> Pathes (S n)
```


#### `param`

``` purescript
param :: forall eff n. Path n -> RoutingM eff (Path n)
```


#### `routes0`

``` purescript
routes0 :: forall eff. Pathes0 -> [Eff eff _] -> RoutingM eff Unit
```


#### `route0`

``` purescript
route0 :: forall eff. Pathes0 -> Eff eff _ -> RoutingM eff Unit
```


#### `routes1`

``` purescript
routes1 :: forall eff. Pathes1 -> [String -> Callback eff Unit] -> RoutingM eff Unit
```


#### `route1`

``` purescript
route1 :: forall eff. Pathes1 -> (String -> Callback eff Unit) -> RoutingM eff Unit
```


#### `routes2`

``` purescript
routes2 :: forall eff. Pathes2 -> [String -> String -> Callback eff Unit] -> RoutingM eff Unit
```


#### `route2`

``` purescript
route2 :: forall eff. Pathes2 -> (String -> String -> Callback eff Unit) -> RoutingM eff Unit
```


#### `routes3`

``` purescript
routes3 :: forall eff. Pathes3 -> [String -> String -> String -> Callback eff Unit] -> RoutingM eff Unit
```


#### `route3`

``` purescript
route3 :: forall eff. Pathes3 -> (String -> String -> String -> Callback eff Unit) -> RoutingM eff Unit
```




