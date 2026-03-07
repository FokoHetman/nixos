{self, inputs, ...}: {
  flake.homeConfigurations.foko = {pkgs, ...}: rec {
    imports = [
      self.homeModules.nixpkgs
      self.homeModules.xdg
      self.homeModules.juna
      self.homeModules.fonts
      self.homeModules.ags
      self.homeModules.lf
      self.homeModules.kitty

      self.homeModules.wallpapers
      self.homeModules.udiskie
      self.homeModules.ghci
      self.homeModules.lwp
      self.homeModules.neofetch
      
      self.homeModules.quickshell
      self.homeModules.hyprlock
      self.homeModules.hyprland
      self.homeModules.tmux
      self.homeModules.git
      self.homeModules.rofi
      self.homeModules.wofi
      self.homeModules.browser
      self.homeModules.bash
    ];
    home.username = "foko";
    home.homeDirectory = "/home/${home.username}";
    
    home.packages = with pkgs; [dconf];
    gtk.enable = true;
    qt.enable = true;
    fonts.fontconfig.enable = true;
    
    programs.home-manager.enable = true;
    systemd.user.startServices = "sd-switch";
    home.stateVersion = "23.11";
  };
  flake.homeConfigurations.foko-small = {...}: rec {
    imports = [
      self.homeModules.nixpkgs
      self.homeModules.xdg
      self.homeModules.juna
      self.homeModules.fonts
      self.homeModules.lf
      self.homeModules.kitty
      self.homeModules.ghci
      self.homeModules.neofetch
      self.homeModules.tmux
      self.homeModules.git
      self.homeModules.bash
    ];
    home.username = "foko";
    home.homeDirectory = "/home/${home.username}";
    fonts.fontconfig.enable = true;
    programs.home-manager.enable = true;
    systemd.user.startServices = "sd-switch";
    home.stateVersion = "23.11";
  };
}
