{inputs, self, ...}: {
  flake.nixosConfigurations.fokopi = inputs.nixpkgs.lib.nixosSystem {
    modules = with self.nixosModules; [
      fokopi-hardware
      fokopi-services
      bootloader-extlinux
      support-ntfs

      dev-haskell

      packages-common-big
      packages-common-small
      packages-fok
      
      openssh
      
      builder-client
      
      nix
      nixpkgs
      
      security
      gnupg
      
      blueman
      pcscd

      user-foko-small
      user-solina
      #user-nathan
      
      default-networking
      internalisation
      state-version

      gitserver
      matrix-server
      fokmail
      hetmanat
      nginx
      torrent-server
      foknet
    ];
  };
  flake.nixosModules.fokopi-hardware = {config, pkgs, lib, ...}: {
    time.timeZone = "Poland";
    networking.hostName = "fokopi";


    hardware.enableRedistributableFirmware = lib.mkDefault true;
    boot.initrd.availableKernelModules = [ "xhci_pci" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ ];
    boot.extraModulePackages = [ ];

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
