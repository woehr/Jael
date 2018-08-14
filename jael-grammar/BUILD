load(
  "//:config.bzl"
, "GHC_PKGS_GRAMMAR"
)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl"
, "haskell_library"
, "haskell_test"
)

filegroup(
  name = "cf"
, srcs = [ "Grammar.cf" ]
)

genrule(
  name = "gen-bnfc"
, srcs = [ ":cf" ]
, outs =
    [ "src/Jael/Grammar/Abs.hs"
    , "src/Jael/Grammar/Lex.x"
    , "src/Jael/Grammar/Par.y"
    , "src/Jael/Grammar/ErrM.hs"
    , "src/Jael/Grammar/Print.hs"
    ]
, cmd = "$(location @bnfc//:bin) --haskell --ghc -p Jael -d --alex3 -o $(@D)/src $<"
, tools = ["@bnfc//:bin"]
)

filegroup(
  name = "bnfc-hs-out"
, srcs =
    [ "src/Jael/Grammar/Abs.hs"
    , "src/Jael/Grammar/ErrM.hs"
    , "src/Jael/Grammar/Print.hs"
    ]
)

genrule(
  name = "gen-alex"
, srcs = [ "src/Jael/Grammar/Lex.x" ]
, outs = [ "src/Jael/Grammar/Lex.hs" ]
, cmd = "$(location @alex//:bin) -o $@ $<"
, tools = ["@alex//:bin"]
)

genrule(
  name = "gen-happy"
, srcs = [ "src/Jael/Grammar/Par.y" ]
, outs = [ "src/Jael/Grammar/Par.hs" ]
, cmd = "$(location @happy//:bin) -gac -o $@ $<"
, tools = ["@happy//:bin"]
)

filegroup(
  name = "src"
, srcs =
    [ "src/Jael/Grammar.hs"
    , ":bnfc-hs-out"
    , ":gen-alex"
    , ":gen-happy"
    ]
)

haskell_library(
  visibility = ["//visibility:public"]
, name = "lib"
, srcs = [ ":src" ]
, src_strip_prefix = "src"
, prebuilt_dependencies = GHC_PKGS_GRAMMAR
)

haskell_test(
  name = "test"
, size = "small"
, data = [ ":src" ]
, srcs = [ "test/Main.hs" ]
, extra_srcs = [ ":cf" ]
, src_strip_prefix = "test"
, prebuilt_dependencies = GHC_PKGS_GRAMMAR
)

filegroup(
  name = "par-y"
, srcs = [ "src/Jael/Grammar/Par.y" ]
)

genrule(
  name = "info"
, srcs = [ ":cf", ":par-y" ]
, outs =
    [ "Par.info"
    , "Par.pretty"
    , "Grammar.tex"
    ]
    , cmd = "$(location @happy//:bin) -i$(@D)/Par.info -p$(@D)/Par.pretty $(location :par-y) && $(location @bnfc//:bin) --latex -p Jael -o $(@D)/ $(location :cf)"
, tools =
    [ "@happy//:bin"
    , "@bnfc//:bin"
    ]
)
