{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-monaco"
, dependencies =
  [ "aff"
  , "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "psci-support"
  , "monaco"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
