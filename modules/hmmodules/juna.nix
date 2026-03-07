{inputs, ...}: {
  flake.homeModules.juna = {...}: {
    imports = [ inputs.juna.homeManagerModules.default ];
    juna = {
      enable = true;
      theme = "gruvbox";
      cli = {
        btop.enable = true;
      };
      desktop = {
        flameshot.enable = true;
        kitty.enable = true;
        gtk.enable = true;
        zathura.enable = true;
      };
    };
  };
}
