{inputs, ...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    xmobar = pkgs.callPackage ../assets/xmonad {shell=false;};
  };
}
