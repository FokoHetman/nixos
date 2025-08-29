{config, lib, pkgs, username, pubKeys, inputs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../../submodules/nginx.nix
    ../../submodules/nathan-overrides.nix
    ../../submodules/nginx.nix
  ];
  boot.loader.grub.enable = lib.mkForce false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;
  
  services.blueman.enable = true;

  home-manager.users."${username}" = import ../../user/${username}/home-mini.nix;
  users.users = {
    jakub = {isNormalUser = true;};
    root = {
    };
    git = {
      isNormalUser = true;
      packages = with pkgs; [
        (pkgs.writeShellScriptBin "gitserver" /*bash*/ '' # $1 - command $2 - 'user/repo' $3 - hash (soon tm)
          case $1 in
            newuser ) mkdir $2;;
            newrepo) git --bare init $2;;
          esac
        '')
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
      ] ++ pubKeys;
    };
    ${username} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      packages = with pkgs; [
        
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
      ] ++ pubKeys;
    };

  };
  environment.systemPackages = with pkgs; [
    inputs.blackmarket.legacyPackages.${pkgs.system}.discord_githook
    (pkgs.writeShellScriptBin "wakethefokup" ''
      ${pkgs.wakeonlan}/bin/wakeonlan 74:56:3c:1b:d0:90 -i 169.254.255.255
    '')
  ];
  services.openssh = {
    enable = true;
    ports = [ 22 2136 ];
    settings = {
      PasswordAuthentication = false;
      AllowUsers = null; # Allows all users by default. Can be [ "user1" "user2" ]
      #UseDns = true;
      #X11Forwarding = true;
      PermitRootLogin = "no"; # "yes", "without-password", "prohibit-password", "forced-commands-only", "no"
      Match = ''
        LocalPort=22
          PasswordAuthentication yes
        Match all
      '';
      AuthorizedKeysFile = "~/.ssh/submitted_authorized_keys";
    };
  };
}
