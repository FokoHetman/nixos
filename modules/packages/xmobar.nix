{inputs, ...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    xmobar = pkgs.callPackage ../assets/xmobar {shell=false;};
  };
}
