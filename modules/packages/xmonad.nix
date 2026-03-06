{inputs, ...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    xmonad = pkgs.callPackage ./xmonad {shell=false;};
  };
}
