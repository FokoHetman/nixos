{config, lib, pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;
  
  services.blueman.enable = true;

  home-manager.users.nathan.stylix.enable = false;
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


}
