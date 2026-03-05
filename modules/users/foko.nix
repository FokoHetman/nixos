{self, inputs, ...}: {
  flake.nixosModules.user-foko = {config, pkgs, lib, ...}: {
    imports = [
      inputs.home-manager.nixosModules.default
    ];
    users.groups.fok = {gid=2137;};
    users.users.foko = {
      isNormalUser = true;
      extraGroups = [ "wheel" "fok" "dialout" "tty" ]; # "firejail"
      packages = with pkgs; [
        self.packages.${pkgs.system}.packettracer
      ];
    };

    home-manager = {
      extraSpecialArgs = { inherit inputs; };
      backupFileExtension = "backup";
    };
    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk ];
    };
    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        self.packages.${pkgs.system}.fonts.rainworld-glyphs
        self.packages.${pkgs.system}.fonts.chakra
      ];
    };
  };
}
