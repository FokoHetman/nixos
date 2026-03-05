{inputs, self, ...}: {
  flake.nixosConfigurations.fokopc = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.nixosModules.fokopc-hardware
      self.nixosModules.bootloader-grub
      self.nixosModules.support-ntfs
      self.nixosModules.packages-common-big
      self.nixosModules.packages-fok
      self.nixosModules.openssh
      self.nixosModules.nixpkgs
      self.nixosModules.user-foko
      self.nixosModules.sops
      #self.nixosModules.user-nathan
    ];
  };
  flake.nixosModules.fokopc-hardware = {config, pkgs, lib, ...}: {
    nixpkgs.config.cudaSupport = true;
    nixpkgs.config.allowUnfree = true;

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

    systemd.services.nvidia-control-devices = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
    };
    networking.interfaces.enp6s0.wakeOnLan.enable=true;
  };
  flake.nixosModules.fokopc-services = {config, pkgs, lib, ...}: {
    services.xserver.videoDrivers = ["nvidia"];
  };
}
