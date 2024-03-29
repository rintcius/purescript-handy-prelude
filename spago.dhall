{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "handy-prelude"
, dependencies =
  [ "aff"
  , "const"
  , "control"
  , "debug"
  , "effect"
  , "functor1"
  , "gen"
  , "ordered-collections"
  , "parallel"
  , "profunctor"
  , "profunctor-lenses"
  , "strings"
  , "these"
  , "transformers"
  , "variant"
  , "arrays"
  , "bifunctors"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "tailrec"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
}
