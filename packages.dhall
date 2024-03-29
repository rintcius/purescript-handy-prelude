let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let overrides = {=}

let additions =
      { functor1 =
        { dependencies = [ "newtype", "prelude" ]
        , repo = "https://github.com/garyb/purescript-functor1.git"
        , version = "v2.0.0"
        }
      }

in  upstream // overrides // additions
