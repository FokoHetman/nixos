{...}: {
  flake.nixosModules.adb = {pkgs, ...}: {
    # this was a program... previously...
    environment.systemPackages = with pkgs; [android-tools];
    users.users.foko.extraGroups = [ "adbusers" ];
  };
}
