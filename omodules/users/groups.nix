{...}: {
  flake.nixosModules.groups = {
    users.groups.fok.gid = 2137;
    users.groups.hetmanat = {};
    users.guest = {};
  };
}
