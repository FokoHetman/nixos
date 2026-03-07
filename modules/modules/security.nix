{...}: {
  flake.nixosModules.security = {...}: {
    security.rtkit.enable = true;
  };
}
