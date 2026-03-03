{inputs, self, ...}: {

  flake.nixosModules.bootloader-grub = { pkgs, config, ...}: {
    boot = {
      loader = {
        grub = {
          enable = true;
          useOSProber = true;
          device = "nodev";
          efiSupport = true;
        };
        efi = {
          canTouchEfiVariables = true;
          efiSysMountPoint = "/boot";
        };
      };
    };
  };
}
