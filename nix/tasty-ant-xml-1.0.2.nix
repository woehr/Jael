{ mkDerivation, base, containers, generic-deriving, ghc-prim, mtl
, stdenv, stm, tagged, tasty, transformers, xml
}:
mkDerivation {
  pname = "tasty-ant-xml";
  version = "1.0.2";
  sha256 = "0pgz2lclg2hp72ykljcbxd88pjanfdfk8m5vb2qzcyjr85kwrhxv";
  libraryHaskellDepends = [
    base containers generic-deriving ghc-prim mtl stm tagged tasty
    transformers xml
  ];
  homepage = "http://github.com/ocharles/tasty-ant-xml";
  description = "Render tasty output to XML for Jenkins";
  license = stdenv.lib.licenses.bsd3;
}
