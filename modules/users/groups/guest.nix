{...}: {
  flake.nixosModules.guest = {...}: {
    users.groups.guest = {};
  };
}
