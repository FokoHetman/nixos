{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    #(import <mobile-nixos/lib/configuration.nix> { device = "pine64-pinephone"; })
    ./hardware-configuration.nix
    #<mobile-nixos/examples/phosh/phosh.nix>
  ];

  nix = {
    #nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
    };
    #settings.secret-key-files = "/etc/nix/private-key";
  };

  networking.hostName = "fokophone";

  #
  # Opinionated defaults
  #
  
  # Use Network Manager
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;
  
  # Use PulseAudio
  #hardware.pulseaudio.enable = true;
  #services.pipewire.enable = false;
  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  
  # Bluetooth audio
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  
  # Enable power management options
  powerManagement.enable = true;
  
  # It's recommended to keep enabled on these constrained devices
  zramSwap.enable = true;

  # Auto-login for phosh
  services.xserver.desktopManager.phosh = {
    user = "foko";
  };

  #
  # User configuration
  #
  
  users.users."foko" = {
    isNormalUser = true;
    description = "Hetman Foko";
    hashedPassword = "$6$67qLdlK6qF7LVn6K$Qo3qlhEAO54Gws8uIRD1mInhgZwj2NM/vC0pqZBferGAACXgJNnMsInwGfGt6d7xQgfaVWwfwRqJeKwxAiFTq.";
    extraGroups = [
      "dialout"
      "feedbackd"
      "networkmanager"
      "video"
      "wheel"
    ];
  };

  environment.systemPackages = with pkgs; [
    feedbackd
    vim
    git
#    (builtins.getFlake "github:FokoHetman/fok-quote")
    inputs.fokquote.packages.${system}.default
  ];
  services = {
    openssh.enable = true;
    tailscale.enable = true;
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
