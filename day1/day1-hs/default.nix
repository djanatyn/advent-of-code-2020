{ haskellPackages }:

haskellPackages.mkDerivation rec {
  pname = "day1-hs";
  version = "0.1.0.0";

  isLibrary = false;
  isExecutable = true;

  executableHaskellDepends = with haskellPackages; [ base ];

  src = builtins.path {
    path = ./.;
    name = pname;
  };

  license = "unknown";
}
