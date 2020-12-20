{ haskellPackages }:

haskellPackages.mkDerivation rec {
  pname = "day5-hs";
  version = "0.1.0.0";

  isLibrary = false;
  isExecutable = true;

  executableHaskellDepends = with haskellPackages; [
    base
    cabal-install
    haskell-language-server
  ];

  src = builtins.path {
    path = ./.;
    name = pname;
  };

  license = "Unlicense";
}
