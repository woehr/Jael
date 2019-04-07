with import ../default.nix {};
hpkgs.shellFor { packages = p: [ jael-types ]; withHoogle = true; }
