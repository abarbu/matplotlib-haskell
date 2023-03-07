{ mkDerivation, ad, aeson, base, bytestring, containers
, deepseq, directory, filepath, lib, process, random
, raw-strings-qq, split, tasty, tasty-expected-failure
, tasty-golden, tasty-hunit, temporary, which, pkgs
}:
let
  python = pkgs.python3.withPackages (p: with p; [
    matplotlib
    numpy
    scipy
  ]);
in
mkDerivation {
  pname = "matplotlib";
  version = "0.7.7";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq filepath process split
    temporary which
  ];
  testHaskellDepends = [
    ad base bytestring directory process random raw-strings-qq split
    tasty tasty-expected-failure tasty-golden tasty-hunit temporary
  ];
  librarySystemDepends = [ python which ];
  testSystemDepends = [ python which ];
  homepage = "https://github.com/abarbu/matplotlib-haskell";
  description = "Bindings to Matplotlib; a Python plotting library";
  license = lib.licenses.bsd3;
}
