{ sources ? import ./nix/sources.nix }:
let
  packages = import sources.nixpkgs { };
  inherit (packages) pkgs;
in rec {
  inherit sources pkgs;

  day1 = {
    haskell = pkgs.callPackage ./day1/day1-hs { };
  };

  day2 = {
    python = pkgs.callPackage ./day2/day2-py { };
  };

  day3 = {
    haskell = pkgs.callPackage ./day3/day3-hs { };
  };

  day4 = {
    haskell = pkgs.callPackage ./day4/day4-hs { };
  };

  day5 = {
    haskell = pkgs.callPackage ./day5/day5-hs { };
  };
}
