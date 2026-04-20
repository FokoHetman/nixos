{inputs, self, ...}: {
  flake.nixosConfigurations.fokolaptop = inputs.nixpkgs.lib.nixosSystem {
    modules = with self.nixosModules; [
      fokolaptop-services
      fokolaptop-hardware
      bootloader-grub

      dev-haskell

      packages-common
      packages-graphical
      packages-fok

      builder-client

      nix
      nixpkgs
      sops

      xserver
      xmonad
      user-foko
      
      openssh
      security
      gnupg
      blueman
      pcscd
      default-networking
      internalisation
      proxychains

      keymanagement
 
      steam

      wireshark
      firejail

      state-version
    ];
  };
  flake.nixosModules.fokolaptop-hardware = {config, pkgs, lib, ...}: {
    hardware.enableRedistributableFirmware = lib.mkDefault true;

    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    fileSystems."/" =
      { device = "/dev/disk/by-uuid/aaa84f77-5c51-46c9-9b87-c0461e582b8c";
        fsType = "ext4";
      };

    fileSystems."/boot" =
      { device = "/dev/disk/by-uuid/F286-F05C";
        fsType = "vfat";
        options = [ "fmask=0077" "dmask=0077" ];
      };

    swapDevices =
      [ { device = "/dev/disk/by-uuid/02de37a1-111f-4d66-94e7-6ffcdf259d18"; }
      ];

    # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
    # (the default) this is the recommended approach. When using systemd-networkd it's
    # still possible to use this option, but it's recommended to use it in conjunction
    # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
    networking.useDHCP = lib.mkDefault true;
    # networking.interfaces.enp2s0.useDHCP = lib.mkDefault true;
    # networking.interfaces.wlp3s0.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
  flake.nixosModules.fokolaptop-services = {config, pkgs, lib, ...}: {
    imports = [self.nixosModules.rainworld];
    services.blueman.enable = true;
    rainworld = {
      enable = true;
      steamkey_path = config.sops.secrets.steam_key.path;
      steamid_path = config.sops.secrets.steam_id.path;
    };
    environment = {
      variables = {
        EDITOR = "nvim";
      };
    };
  };
}
