load(
  "//:config.bzl"
, "GHC_VER"
)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl"
, "haskell_toolchain"
)

haskell_toolchain(
  name = "ghc"
, version = GHC_VER
, tools = "@ghc//:bin"
)
