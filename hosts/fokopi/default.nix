{config, lib, pkgs, username, pubKeys, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../../submodules/nathan-overrides.nix
  ];
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;
  
  services.blueman.enable = true;

  users.users = {
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
    };
    ${username} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      packages = with pkgs; [
        
      ];
    };

  };
  
  users.users.${username}.openssh.authorizedKeys.keys = [

  ] ++ pubKeys;
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
    };
  };
}
