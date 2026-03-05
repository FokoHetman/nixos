{...}:
{
  flake.nixosModules.wireshark = {...}: {
    wireshark.enable = true;
    users.users.foko.extraGroups = ["wireshark"];
  };
}
