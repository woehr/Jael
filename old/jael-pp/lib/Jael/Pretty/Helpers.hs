{-# LANGUAGE OverloadedStrings #-}

module Jael.Pretty.Helpers where

import           Data.Text.Prettyprint.Doc

-- for Variable Character
vc :: Doc ann
vc = "$"

myFlatAlt :: Doc ann -> Doc ann -> (Doc ann, Doc ann)
myFlatAlt l r = (flatAlt (l <> space) l, flatAlt (hardline <> r) r)

ppApp :: (Pretty a) => a -> [Doc ann] -> Doc ann
ppApp a = (pretty a <>) . align . ppParens

ppParens :: [Doc ann] -> Doc ann
ppParens =
  group . encloseSep (flatAlt "( " "(") (flatAlt (hardline <> ")") ")") ", "

ppBrackets :: [Doc ann] -> Doc ann
ppBrackets =
  group . encloseSep (flatAlt "[ " "[") (flatAlt (hardline <> "]") "]") ", "

ppBraces :: [Doc ann] -> Doc ann
ppBraces =
  group . encloseSep (flatAlt "{ " "}") (flatAlt (hardline <> "}") "}") ", "

ppRec :: [Doc ann] -> Doc ann
ppRec = group . uncurry encloseSep (myFlatAlt lbrace rbrace) ", "

ppRow :: (Pretty l) => (l, Doc ann) -> Doc ann
ppRow (l, p) = pretty l <+> equals <+> p

ppBracesPipe :: [Doc ann] -> Doc ann -> Doc ann
ppBracesPipe xs y = group
  (uncurry encloseSepTail
           (myFlatAlt lbrace rbrace)
           ", "
           (flatAlt "| " " | ")
           xs
           y
  )

encloseSepTail
  :: Doc ann -> Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann -> Doc ann
encloseSepTail l r s t xs y =
  cat (zipWith (<>) (l : repeat s) xs <> [t <> y]) <> r
