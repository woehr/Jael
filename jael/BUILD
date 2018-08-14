load(
  "//:config.bzl"
, "GHC_PKGS"
)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_binary",
  "haskell_test",
)

haskell_library(
  name = "lib"
, srcs = glob([ "lib/**/*.hs" ])
, src_strip_prefix = "lib"
, deps = [ "//jael-grammar:lib" ]
, prebuilt_dependencies = GHC_PKGS
)

haskell_binary(
  name = "jaelc"
, srcs = [ "src/Main.hs" ]
, src_strip_prefix = "src"
, deps = [ ":lib" ]
, prebuilt_dependencies = GHC_PKGS
)

filegroup(
  name = "test-inputs"
, srcs =
    [ "test/infer.j"
    ]
)

haskell_test(
  name = "test"
, size = "small"
, data = [ ":test-inputs" ]
, args = [ "$(location :test-inputs)" ]
, srcs = glob(["test/Jael/*.hs", "test/Config.hs", "test/Spec.hs", "test/Main.hs" ])
, src_strip_prefix = "test"
, deps = [ ":lib" ]
, prebuilt_dependencies = GHC_PKGS
)
#"@hspec-discover//:bin"
