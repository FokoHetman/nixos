{inputs, self, ...}: {
  flake.nixosConfigurations.fokopi = inputs.nixpkgs.lib.nixosSystem {
    modules = self.packages [
      self.nixosModules.fokopi-hardware
      self.nixosModules.bootloader-grub
      self.nixosModules.support-ntfs
      self.nixosModules.packages-common-big
      self.nixosModules.packages-fok
      self.nixosModules.openssh
      self.nixosModules.nixpkgs
      #self.nixosModules.user-foko
      #self.nixosModules.user-nathan
    ];
  };
  flake.nixosModules.fokopi-hardware = {config, pkgs, lib, ...}: {
    hardware.enableRedistributableFirmware = lib.mkDefault true;
    boot.initrd.availableKernelModules = [ "xhci_pci" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ ];
    boot.extraModulePackages = [ ];

    boot.loader.grub.enable = lib.mkForce false;
    boot.loader.generic-extlinux-compatible.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_rpi4;

    fileSystems."/" =
      { device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
        fsType = "ext4";
      };

    swapDevices = [ ];

    networking.useDHCP = lib.mkDefault true;
    nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
  };
  flake.nixosModules.fokopi-services = {config, pkgs, lib, ...}: {
    environment.systemPackages = [
      inputs.blackmarket.legacyPackages.${pkgs.system}.discord_githook
      (pkgs.writeShellScriptBin "wakethefokup" ''
        ${pkgs.wakeonlan}/bin/wakeonlan 74:56:3c:1b:d0:90 -i 169.254.255.255
      '')
    ];
  };
}
