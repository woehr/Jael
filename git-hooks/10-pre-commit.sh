#!/bin/sh

CABAL_INPUT_MD5=$(md5sum package.yaml | cut -f1 -d' ')
CABAL_OUTPUT_MD5=$(grep -Po -m 1 'Input-MD5: \K[0-9a-f]{32}' jael.cabal)

#LEXER_INPUT_MD5=$(md5sum lib/Jael/Grammar/Lexer.x | cut -f1 -d' ')
#LEXER_OUTPUT_MD5=$(grep -Po -m 1 'Input-MD5: \K[0-9a-f]{32}' lib/Jael/Grammar/Lexer.hs)

#PARSER_INPUT_MD5=$(md5sum lib/Jael/Grammar/Parser.y | cut -f1 -d' ')
#PARSER_OUTPUT_MD5=$(grep -Po -m 1 'Input-MD5: \K[0-9a-f]{32}' lib/Jael/Grammar/Parser.hs)

if [ "$CABAL_INPUT_MD5" != "$CABAL_OUTPUT_MD5" ]
then
  echo "MD5 of package.yaml does not match the MD5 in jael.cabal"
  echo "$CABAL_INPUT_MD5" != "$CABAL_OUTPUT_MD5"
  exit 1
fi

#if [ "$LEXER_INPUT_MD5" != "$LEXER_OUTPUT_MD5" ]
#then
#  echo "MD5 of Lexer.x does not match the MD5 in Lexer.hs"
#  echo "$LEXER_INPUT_MD5" != "$LEXER_OUTPUT_MD5"
#  exit 1
#fi

#if [ "$PARSER_INPUT_MD5" != "$PARSER_OUTPUT_MD5" ]
#then
#  echo "MD5 of Parser.y does not match the MD5 in Parser.hs"
#  echo "$PARSER_INPUT_MD5" != "$PARSER_OUTPUT_MD5"
#  exit 1
#fi
