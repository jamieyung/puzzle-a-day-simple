{ name = "puzzle-a-day-simple"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "newtype"
  , "prelude"
  , "strings"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
