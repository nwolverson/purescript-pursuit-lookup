{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pursuit-lookup"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "argonaut"
  , "arrays"
  , "either"
  , "maybe"
  , "media-types"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
