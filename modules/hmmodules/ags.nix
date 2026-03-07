{inputs, ...}: {
  flake.homeModules.ags = {pkgs, ...}: {
    imports = [
      inputs.ags.homeManagerModules.default
    ];
    programs.ags = {
      enable = true;
      configDir = ./ags;
      extraPackages = with pkgs; [
        gtksourceview
        webkitgtk_6_0
        accountsservice
      ];
    };
  };
}
