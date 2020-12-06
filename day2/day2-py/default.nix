{ python38Packages }:

python38Packages.buildPythonApplication rec {
  pname = "day2-py";
  version = "1.0.0";

  src = builtins.path {
    path = ./.;
    name = pname;
  };
}
