# Module Documentation

## Module Network.Routing.Client

### Types


    type EffRouting eff = Eff (routing :: Routing | eff)


    data Path n


    newtype Pathes n


    type Pathes0 = Pathes Z


    type Pathes1 = Pathes (S Z)


    type Pathes2 = Pathes (S (S Z))


    type Pathes3 = Pathes (S (S (S Z)))


    data Router :: *


    data Routing :: !


    newtype RoutingM eff a


    data S n


    type SetRoute eff = String -> EffRouting eff Unit


    data Z


### Type Class Instances


    instance applicativeRoutingM :: Applicative (RoutingM eff)


    instance applyRoutingM :: Apply (RoutingM eff)


    instance bindRoutingM :: Bind (RoutingM eff)


    instance functorRoutingM :: Functor (RoutingM eff)


    instance monadRoutingM :: Monad (RoutingM eff)


    instance showPath :: Show (Path n)


    instance showPathes :: Show (Pathes n)


### Values


    (+/) :: forall n. Path (S Z) -> Pathes n -> Pathes (S n)


    (-/) :: forall n. Path Z -> Pathes n -> Pathes n


    any :: Path (S Z)


    empty :: Pathes0


    exact :: String -> Path Z


    getRouter :: forall eff. RoutingM eff Router


    notFound :: forall eff. (SetRoute eff -> EffRouting eff _) -> RoutingM eff Unit


    param :: forall eff n. Path n -> RoutingM eff (Path n)


    regex :: String -> Path (S Z)


    route0 :: forall eff. Pathes0 -> EffRouting eff _ -> RoutingM eff Unit


    route1 :: forall eff. Pathes1 -> (String -> EffRouting eff _) -> RoutingM eff Unit


    route2 :: forall eff. Pathes2 -> (String -> String -> EffRouting eff _) -> RoutingM eff Unit


    route3 :: forall eff. Pathes3 -> (String -> String -> String -> EffRouting eff _) -> RoutingM eff Unit


    routes0 :: forall eff. Pathes0 -> [EffRouting eff _] -> RoutingM eff Unit


    routes1 :: forall eff. Pathes1 -> [String -> EffRouting eff _] -> RoutingM eff Unit


    routes2 :: forall eff. Pathes2 -> [String -> String -> EffRouting eff _] -> RoutingM eff Unit


    routes3 :: forall eff. Pathes3 -> [String -> String -> String -> EffRouting eff _] -> RoutingM eff Unit


    runRouter :: forall eff. RoutingM eff _ -> EffRouting eff (SetRoute eff)


    unsafeGlobalRoute :: forall eff. RoutingM eff _ -> SetRoute eff


    useHistoryAPI :: RoutingM _ Unit



