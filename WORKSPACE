workspace(name = "Jael")

load(
  "//:config.bzl"
, "RULES_HASKELL_VER"
, "RULES_NIXPKGS_VER"
, "NIXPKGS_VER"
, "GHC_VER"
, "GHC_VER_NIX"
, "GHC_PKGS"
)

http_archive(
  name = "io_tweag_rules_haskell"
, strip_prefix = "rules_haskell-" + RULES_HASKELL_VER
, urls = ["https://github.com/tweag/rules_haskell/archive/"
          + RULES_HASKELL_VER + ".tar.gz"]
)

load(
  "@io_tweag_rules_haskell//haskell:repositories.bzl"
, "haskell_repositories"
)

haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs"
, strip_prefix = "rules_nixpkgs-" + RULES_NIXPKGS_VER
, urls = ["https://github.com/tweag/rules_nixpkgs/archive/"
          + RULES_NIXPKGS_VER + ".tar.gz"]
)

load(
  "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl"
, "nixpkgs_git_repository"
, "nixpkgs_package"
)

nixpkgs_git_repository(
  name = "nixpkgs"
, revision = NIXPKGS_VER
)

nixpkgs_package(
  name = "ghc"
, repository = "@nixpkgs"
, nix_file_deps = [ "//:pinned-nixpkgs.nix" ]
, nix_file_content =
  """
    (import ./pinned-nixpkgs.nix).haskell.packages.{ghc}.ghcWithPackages (p:
        with p; [ {pkgs} ] )
  """.format(ghc=GHC_VER_NIX, pkgs=" ".join(GHC_PKGS))
)

nixpkgs_package(
  name = "bnfc"
, attribute_path = "haskell.packages.{ghc}.BNFC".format(ghc=GHC_VER_NIX)
, repository = "@nixpkgs"
)

nixpkgs_package(
  name = "alex"
, attribute_path = "haskell.packages.{ghc}.alex".format(ghc=GHC_VER_NIX)
, repository = "@nixpkgs"
)

nixpkgs_package(
  name = "happy"
, attribute_path = "haskell.packages.{ghc}.happy".format(ghc=GHC_VER_NIX)
, repository = "@nixpkgs"
)

nixpkgs_package(
  name = "hspec-discover"
, attribute_path = "haskell.packages.{ghc}.hspec-discover".format(ghc=GHC_VER_NIX)
, repository = "@nixpkgs"
)

register_toolchains("//:ghc")
