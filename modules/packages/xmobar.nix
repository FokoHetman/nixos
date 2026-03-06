{inputs, ...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    xmobar = pkgs.callPackage ./xmobar {shell=false;};
  };
}
