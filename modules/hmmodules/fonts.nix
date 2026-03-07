{self, ...}: {
  flake.homeModules.fonts = {pkgs, ...}: {
    home.packages = builtins.attrValues self.packages.${pkgs.system}.fonts ++ (with pkgs; [nerd-fonts.fira-code nerd-fonts.droid-sans-mono]);
  };
}
