#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "(import (fetchTarball https://channels.nixos.org/nixos-19.03/nixexprs.tar.xz) {}).haskellPackages.hpack-dhall"

find "$(dirname "$(readlink -f "$0")")" -name package.dhall -exec dhall-hpack-cabal --package-dhall {} \;
