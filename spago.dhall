{ name = "Lunartime"
, dependencies =
  [ "concur-core"
  , "concur-react"
  , "console"
  , "debugged"
  , "effect"
  , "generics-rep"
  , "prelude"
  , "psci-support"
  ]
, sources = [ "src/**/*.purs" ]
, packages = ./packages.dhall
}
