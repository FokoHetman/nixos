{...}: {
  flake.nixosModules.adb = {...}: {
    users.users.foko.extraGroups = [ "adbusers" ];
    programs.adb.enable = true;
  };
}
