{inputs,...}: {
  flake.nixosModules.sops = {pkgs,...}: {
    imports = [inputs.sops-nix.nixosModules.sops];
  };
}
