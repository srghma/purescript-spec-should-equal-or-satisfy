{ name = "spec-should-equal-deep"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "contravariant"
  , "foldable-traversable"
  , "typelevel-prelude"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
