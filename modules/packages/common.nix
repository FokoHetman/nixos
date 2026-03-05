{inputs, self, ...}:
let 
  common = pkgs: with pkgs; [
    pkg-config ripgrep neofetch pinentry-curses vim
    self.packages.${pkgs.system}.nvim
    self.packages.${pkgs.system}.ffmpegxcb
  ];
  big = pkgs: with pkgs; [
    bluez
    asciinema_3
    alsa-lib
    texlab 
  ];
in {
  flake.nixosModules.packages-common = { pkgs, ...}: {
    environment.systemPackages = common pkgs;
  };
  flake.nixosModules.packages-common-big = { pkgs, ...}: {
    environment.systemPackages = common pkgs ++ big pkgs;
  };
}
