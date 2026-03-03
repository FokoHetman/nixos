{...}: {
  flake.nixosModules.builder = {config, pkgs, lib, ...}: {
    users.groups.builder = {};
  };
}
