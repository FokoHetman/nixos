{lib, ...}: {

  flake.nixosModules.bootloader-extlinux = { pkgs, config, ...}: {
    boot.loader.grub.enable = lib.mkForce false;
    boot.loader.generic-extlinux-compatible.enable = true;
  };
}
