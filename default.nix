{ sources ? import ./nix/sources.nix }:
let
  packages = import sources.nixpkgs { };
  inherit (packages) pkgs;
in rec {
  inherit sources;

  day1 = {
    haskell = pkgs.callPackage ./day1/day1-hs { };
  };
}
