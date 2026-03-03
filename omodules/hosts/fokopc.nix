{inputs, self, ...}: {
  flake.nixosConfigurations.fokopc = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.nixosModules.fokopc-hardware  ##
      self.nixosModules.bootloader-grub  ##
      self.nixosModules.support-ntfs     ##
      self.nixosModules.packages-common-big  ##
      self.nixosModules.packages-fok     ##
      self.nixosModules.user-foko
      self.nixosModules.user-nathan
    ];
  };

  flake.nixosModules.fokopc-hardware = {config, pkgs, lib, ...}: {
    security.rtkit.enable = true;
    hardware.enableRedistributableFirmware = lib.mkDefault true;
    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];
    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

    fileSystems = {
      "/" = {
        device = "/dev/disk/by-uuid/b7a4d864-4427-45e6-8e37-5f0897b708bb";
        fsType = "ext4";
      };
      "/boot" = {
        device = "/dev/disk/by-uuid/BC85-C80C";
        fsType = "vfat";
        options = [ "fmask=0022" "dmask=0022" ];
      };
      # ...
      "/nix" = {
        device = "/dev/disk/by-label/nix";
        fsType = "ext4";
        neededForBoot = true;
        options = [ "noatime" ];
      };
      "/mnt/games" = {
        device = "/dev/disk/by-label/nix";
        fsType = "ext4";
      };
    };
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };
    hardware.bluetooth.enable = true;
    hardware.bluetooth.powerOnBoot = true;
    hardware.nvidia = {
      modesetting.enable=true;
      powerManagement.enable = false;
      powerManagement.finegrained=false;
      open=false;
      nvidiaSettings=true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };
    swapDevices = [ ];
    networking.useDHCP = lib.mkDefault true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
