#! /usr/bin/env bash
# Note: The line below is probably required for ghcide to work properly
# export NIX_GHC_LIBDIR=$(ghc --print-libdir)
nix-env -f ./deps.nix -i '.*'
