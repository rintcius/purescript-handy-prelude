{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "handy-prelude"
, dependencies =
  [ "aff"
  , "console"
  , "const"
  , "control"
  , "debug"
  , "effect"
  , "functor1"
  , "gen"
  , "generics-rep"
  , "ordered-collections"
  , "parallel"
  , "profunctor"
  , "profunctor-lenses"
  , "strings"
  , "these"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
}
