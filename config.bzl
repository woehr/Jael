RULES_HASKELL_VER = "35c22a2ab758ace2be4bbaa36c96664c18b7a5ac"
RULES_NIXPKGS_VER = "896c2d96a70a408c545cc491974068c36f507009"

#NIXPKGS_VER = "49d97c34958c1b31c509fe1cb52d97c998bd6184"
#NIXPKGS_VER = "a2338472fbab3af17842ccde36ab0b055ce40ec8"
# 2018/07/10
#NIXPKGS_VER = "0f50f43f251b31de90b633cafea00d83563cc4c8"
# 2018/08/11
NIXPKGS_VER = "3fe07514949226ad74b9c35180bfb6ebb630fa20"

GHC_VER = "8.2.2"
#GHC_VER = "8.6.1"
GHC_VER_NIX = "ghc" + "".join(GHC_VER.split("."))

# One package per line
GHC_PKGS_GRAMMAR = [x.strip() for x in """
  directory
  array
  base
  bytestring
  containers
  file-embed
  hint
  hspec
  split
""".split("\n") if x and not x.isspace()]

#  liquid-fixpoint
#  llvm-hs
#  llvm-hs-pure
GHC_PKGS_LIB = [x.strip() for x in """
  base
  bytestring
  cmdargs
  comonad
  containers
  deriving-compat
  discrimination
  dlist
  file-embed
  free
  haskus-utils
  hspec
  hspec-discover
  lens
  mtl
  multimap
  multiset
  placeholders
  pretty-simple
  pretty-tree
  prettyprinter
  prettyprinter-ansi-terminal
  QuickCheck
  recursion-schemes
  regex-applicative
  row-types
  safe
  split
  syb
  template-haskell
  text
  these
  tostring
  tree-diff
  uniform-pair
  vec
""".split("\n") if x and not x.isspace()]

# List of packages less the don't check packages
GHC_PKGS = GHC_PKGS_GRAMMAR + GHC_PKGS_LIB
