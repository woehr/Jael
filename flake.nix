{
  description = "Jael flake";

  inputs = {
    nixpkgs.url = "nixpkgs/release-21.11";
    utils.url = github:gytis-ivaskevicius/flake-utils-plus/v1.3.1;
  };

  outputs = inputs@{ self, utils, ... }:
  let pkgs = self.pkgs.x86_64-linux.nixpkgs;
      hpkgs = pkgs.haskell.packages.ghc8107;
      jael = hpkgs.callCabal2nix "jael" ./. {};
      ghc = hpkgs.ghcWithPackages ( p: jael.buildInputs );

  in utils.lib.mkFlake {
    inherit self inputs;

    supportedSystems = [ "x86_64-linux" ];
    #channelsConfig.allowUnfree = true;

    defaultPackage = jael;

    devShell.x86_64-linux = pkgs.mkShell {
      inputsFrom = [ jael ];
      shellHook = ''
        export NIX_GHC_LIBDIR=$(ghc --print-libdir)
      '';
    };

    apps = {
      install-git-hooks = utils.lib.mkApp {
        drv = pkgs.writeScriptBin "jael-install-git-hooks" ''
          ln -s git-hooks/10-pre-commit.sh .git/hooks/pre-commit
        '';
      };

      gen-files = utils.lib.mkApp {
        drv = pkgs.writeScriptBin "jael-gen-files" ''
          ${hpkgs.hpack}/bin/hpack --hash || exit 1
          ${hpkgs.hpack}/bin/hpack --hash -f

          CABAL_MD5=$(${pkgs.coreutils}/bin/md5sum package.yaml)
          ${pkgs.gnused}/bin/sed -i "0,/^--.*/s;^--.*;-- Input-MD5: $CABAL_MD5\n&;" jael.cabal
        '';
      };
    };
  };
}

          #LEXER_MD5=$(${pkgs.coreutils}/bin/md5sum lib/Jael/Grammar/Lexer.x)
          #${hpkgs.alex}/bin/alex -o lib/Jael/Grammar/Lexer.hs -ilexer.info -g -s 2 lib/Jael/Grammar/Lexer.x
          #${pkgs.gnused}/bin/sed -i "1s;^;-- Input-MD5: $LEXER_MD5\n;" lib/Jael/Grammar/Lexer.hs

          #PARSER_MD5=$(${pkgs.coreutils}/bin/md5sum lib/Jael/Grammar/Parser.y)
          #${hpkgs.happy}/bin/happy -o lib/Jael/Grammar/Parser.hs -iparser.info -pparser.pretty -g lib/Jael/Grammar/Parser.y
          #${pkgs.gnused}/bin/sed -i "1s;^;-- Input-MD5: $PARSER_MD5\n;" lib/Jael/Grammar/Parser.hs
