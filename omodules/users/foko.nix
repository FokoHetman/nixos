{inputs, self, ...}: {
  flake.nixosModules.user-foko = {config, pkgs, lib, ...}: {
    users.users.foko = {
      isNormalUser = true;
      extraGroups = [ "wheel" "fok" "dialout" "firejail" "tty" "wireshark" ];
    };
  };
}
