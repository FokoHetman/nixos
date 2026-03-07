{...}:
{
  flake.nixosModules.firejail = {...}: {
    programs.firejail.enable = true;
    users.users.foko.extraGroups = ["firejail"];
  };
}
