{ shell ? false }:
let
  lib-name = "libjael";

  pkgs = import ../pinned-nixpkgs.nix;
  inherit (pkgs) stdenv writeText;
  inherit (import ../config-vars.nix) GHC_VER_NIX GHC_PKGS_LIB;
  hlib = pkgs.haskell.lib;
  inherit (pkgs.lib) concatStringsSep flatten inNixShell isList mapAttrs
                     mapAttrsRecursive mapAttrsToList replaceStrings;

  f = rec {
    recDirRead = dir: k: v:
      if v == "directory"
        then let dir' = "/" + dir + "/" + k;
             in mapAttrs (recDirRead dir') (builtins.readDir dir')
        else k
    ;
    recConcatList = k: v:
      if isList v
        then concatStringsSep "." v
        else mapAttrsToList recConcatList v
    ;
  };

  allHsModules = dir:
    let x = mapAttrs (f.recDirRead dir) (builtins.readDir dir);
        x' = mapAttrsRecursive (xs: k: xs) x;
        x'' = mapAttrsToList f.recConcatList x';
    in map (replaceStrings [".hs"] [""]) (flatten x'');

  grammar-lib = import ../jael-grammar/default.nix {};

  cabal-file = import ../cabal.nix {
    top = {
      name = lib-name;
      version = "0.0.0.0";
    };
    lib = {
      build-depends = concatStringsSep "," (GHC_PKGS_LIB ++ ["libjaelg"]);
      exposed-modules = concatStringsSep " " (allHsModules ./lib);
      hs-source-dirs = "lib";
    };
    exes = [{
      name = "jaelc";
      build-depends = "base, butcher, containers, libjael, text";
      hs-source-dirs = "src";
    }];
    tests = [
      { name = "tests";
        build-depends = concatStringsSep "," (GHC_PKGS_LIB ++ ["libjael" "libjaelg"]);
        hs-source-dirs = "test";
      }
    ];
  };

  libjael-src = stdenv.mkDerivation {
    name = "${lib-name}-src";
    srcs = [ ./lib ./src ./test cabal-file ];
    phases = [ "buildPhase" "installPhase" ];
    buildPhase =
    ''
      source $stdenv/setup

      for f in $srcs; do
        cp -r --no-preserve=mode $f $(basename $f | cut -d- -f2)
      done
    '';
    installPhase = "cp -r . $out";
  };

  h = #if shell
       # then pkgs.haskell.packages."${GHC_VER_NIX}".override {
        #  overrides = self: super: {
            #ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
            #ghcWithPackages = self.ghc.withPackages;

            #ListLike = pkgs.haskell.lib.doJailbreak super.ListLike;
            #stm-containers = pkgs.haskell.lib.doJailbreak super.stm-containers;
            #llvm-hs = super.llvm-hs.override { llvm-config = pkgs.llvm_6; };
         # };
       # }
        pkgs.haskell.packages."${GHC_VER_NIX}";
  jael-pkg = h.callCabal2nix "libjael" "${libjael-src}" { libjaelg = grammar-lib; };

in if shell
  then
    (hlib.addBuildDepends jael-pkg (
      with h; [ cabal-install ])
#      with h; [ brittany ghc-mod ghcid hlint ])
    ).env
  else
    jael-pkg
