#! /bin/sh -eu

mkdir -p "$PWD/.gcroots"
nix-instantiate "$PWD/shell.nix" --indirect --add-root "$PWD/.gcroots/shell.drv"
nix-store --indirect --add-root "$PWD/.gcroots/shell.dep" \
          --realise $(nix-store --query --references "$PWD/.gcroots/shell.drv")
exec nix-shell "$(readlink "$PWD/.gcroots/shell.drv")"
