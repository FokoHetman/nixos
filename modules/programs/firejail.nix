{...}:
{
  flake.nixosModules.firejail = {...}: {
    firejail.enable = true;
    users.users.foko.extraGroups = ["firejail"];
  };
}
