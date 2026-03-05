{...}: {
  flake.nixosModules.nixpkgs = {...}: {
    nixpkgs.config.allowUnfree = true;
  };
}
