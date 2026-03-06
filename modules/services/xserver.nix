{lib, ...}: {
  flake.nixosModules.xserver = {
    services.xserver = {
      enable = true;
      xkb.layout = "pl,ru";
      displayManager = lib.mkDefault {
        startx.enable = true;
        sx.enable = true;
      };
    };
  };
}
