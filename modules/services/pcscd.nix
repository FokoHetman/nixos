{...}: {
  flake.nixosModules.pcscd = {...}: {
    services.pcscd.enable = true;
  };
}
