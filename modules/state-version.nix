{...}: {
  flake.nixosModules.state-version = {...}: {
    system.stateVersion = "23.11";
  };
}
