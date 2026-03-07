{...}: {
  flake.nixosModules.steam = {pkgs, ...}: {
     sops.secrets.steam_key = { owner = "foko"; };
    sops.secrets.steam_id  = { owner = "foko"; };
    programs = {
      steam = {
        enable = true;
        gamescopeSession.enable = true;
        extraCompatPackages = with pkgs; [
          proton-ge-bin
        ];
      };
      gamemode.enable = true;
      gamescope.enable = true;
    };
  };
}
