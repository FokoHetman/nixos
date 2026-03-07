{...}:
{
  flake.nixosModules.wireshark = {...}: {
    programs.wireshark.enable = true;
    users.users.foko.extraGroups = ["wireshark"];
  };
}
