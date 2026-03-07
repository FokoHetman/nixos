{self, inputs, ...}: {
  flake.nixosModules.user-foko = {config, pkgs, lib, ...}: {
    imports = [
      inputs.home-manager.nixosModules.default
    ];
    users.groups.fok = {gid=2137;};
    services.openssh.settings.AllowUsers = ["foko"];
    users.users.foko = {
      isNormalUser = true;
      extraGroups = [ "wheel" "fok" "dialout" "tty" ]; # "firejail"
      packages = with pkgs; [
      ];
      openssh.authorizedKeys.keys = self.globals.fokossh ++ self.globals.nathanssh;
      hashedPassword = "$6$eWtzd1nfbaj3nYIa$IKhWPIuiSjI/WuVOupf1FNsvbQjIvT1HrfFUOjsDMT0nwBZHp3VRjwqOegFsO7IJfQLmkV/YPEbyJN.2u23pd/";
    };

    home-manager = {
      extraSpecialArgs = { inherit inputs; };
      backupFileExtension = "backup";
    };
    home-manager.users.foko = self.homeConfigurations.foko;
    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk ];
    };
    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        self.packages.${pkgs.system}.fonts.rainworld
        self.packages.${pkgs.system}.fonts.chakra
      ];
    };
  };

  flake.nixosModules.user-foko-small = {config, pkgs, lib, ...}: {
    imports = [
      inputs.home-manager.nixosModules.default
    ];
    users.groups.fok = {gid=2137;};
    services.openssh.settings.AllowUsers = ["foko"];
    users.users.foko = {
      isNormalUser = true;
      extraGroups = [ "wheel" "fok" "dialout" "tty" ]; # "firejail"
      packages = with pkgs; [
      ];
      openssh.authorizedKeys.keys = self.globals.fokossh ++ self.globals.nathanssh;
      hashedPassword = "$6$eWtzd1nfbaj3nYIa$IKhWPIuiSjI/WuVOupf1FNsvbQjIvT1HrfFUOjsDMT0nwBZHp3VRjwqOegFsO7IJfQLmkV/YPEbyJN.2u23pd/";
    };

    home-manager = {
      extraSpecialArgs = { inherit inputs; };
      backupFileExtension = "backup";
    };
    home-manager.users.foko = self.homeConfigurations.foko-small;
    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk ];
    };
    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        self.packages.${pkgs.system}.fonts.rainworld
        self.packages.${pkgs.system}.fonts.chakra
      ];
    };
  };
}
