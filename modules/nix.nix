{inputs, ...}: {
  flake.nixosModules.nix = {...}: {
    nix = {
      nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
      settings = {
        trusted-users = ["@wheel"];
        experimental-features = [ "nix-command" "flakes" "pipe-operators"];
      };
      settings.secret-key-files = "/etc/nix/private-key";
    };
  };
}
