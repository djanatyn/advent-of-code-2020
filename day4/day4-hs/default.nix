{ haskellPackages }:

haskellPackages.mkDerivation rec {
  pname = "day4-hs";
  version = "0.1.0.0";

  isLibrary = false;
  isExecutable = true;

  executableHaskellDepends = with haskellPackages; [
    base
    cabal-install
    megaparsec
    text
    parser-combinators
  ];

  src = builtins.path {
    path = ./.;
    name = pname;
  };

  license = "Unlicense";
}
