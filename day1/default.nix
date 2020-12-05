{ haskellPackages, lib }:

{
  haskell = haskellPackages.mkDerivation rec {
    pname = "day1-hs";
    version = "0.1.0.0";

    isLibrary = false;
    isExecutable = true;

    executableHaskellDepends = with haskellPackages; [ base ];

    src = builtins.path {
      path = ./day1-hs/.;
      name = pname;
    };

    license = with lib.licenses; unlicense
  };
}
