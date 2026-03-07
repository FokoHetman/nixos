{lib, self, inputs, ...}: {
  flake.homeModules.lwp = {pkgs, ...}: {
    home.packages = [
      inputs.blackmarket.legacyPackages.${pkgs.system}.lwp
    ];
    home.file.".config/lwp/wallpapers" = {
      source = ./dots/lwp/wallpapers;
    };
  };
}
