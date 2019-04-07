#! /usr/bin/env bash
nix-shell -p '(import ../pinned.nix).haskell.packages.ghc822.ghcWithPackages (p: with p; [containers ghcid liquidhaskell])' --run "ghcid -T \":! liquid $1\" \"$1\""
