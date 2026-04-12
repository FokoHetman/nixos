{inputs, ...}: let username = "foko"; in {
  flake.nixosModules.sops = {pkgs,config,...}: {
    imports = [inputs.sops-nix.nixosModules.sops];
    sops.defaultSopsFile = ./secrets.yaml;
    sops.defaultSopsFormat = "yaml";
    sops.age.keyFile = "/home/${username}/.config/sops/age/keys.txt";
    sops.secrets.shrimp = { owner = username; };
    sops.secrets.ds_token = { owner = username; };
    sops.secrets.fok = {};
    environment.systemPackages = with pkgs; [
      sops
    ];
  };
}
