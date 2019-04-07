#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "(import ../pinned.nix).haskell.packages.ghc822.BNFC"
set -e

mkdir -p src
mkdir -p lib/Jael/Grammar

bnfc --haskell --ghc -p Jael -d --alex3 -o lib ../jael-grammar-test/Grammar.cf

rm lib/Jael/Grammar/Skel.hs
rm lib/Jael/Grammar/Doc.txt
mv lib/Jael/Grammar/Test.hs src/Main.hs
sed -i "/import Jael.Grammar.Skel/d" src/Main.hs
