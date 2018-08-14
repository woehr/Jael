# Any version of nixpkgs can be used to create this set. The pinned version
# of nixpkgs in ./pinned-nixpkgs.nix should be used for everything else.
with (import <nixpkgs> {}); with stdenv;
let
  bzl2nix = writeScript "bzl2nix" ''
    import os
    import json
    if __name__ == '__main__':
      src = os.environ['src']
      out = os.environ['out']
      vars = dict()
      exec(open(src, 'r').read(), vars)
      vars.pop('__builtins__')
      with open(out, 'w') as f:
        json.dump(vars, f)
  '';
  drv = mkDerivation {
    name = "config.nix";
    src = ./config.bzl;
    builder = writeScript "bzl2nix" ''
      source $stdenv/setup
      ${pkgs.python3}/bin/python3 ${bzl2nix}
    '';
  };
in with builtins; fromJSON (readFile "${drv}")
