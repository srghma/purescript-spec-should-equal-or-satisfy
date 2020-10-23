{ name = "spec-should-equal-deep"
, dependencies = [ "console", "effect", "psci-support", "spec", "protolude", "spec-discovery", "contravariant", "foldable-traversable" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
