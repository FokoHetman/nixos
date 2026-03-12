{inputs, self, ...}:
let 
  common = pkgs: with pkgs; [
    fastfetch
    lsd
    hashcat
    bluez
    nixd
    pkg-config pinentry-curses vim bat
    self.packages.${pkgs.system}.nvim
    self.packages.${pkgs.system}.ffmpegxcb
    nmap wget git curl
    zip unzip
    ncurses
    nasm gcc rustc cargo zig

    self.packages.${pkgs.system}.ns
  ];
  big = pkgs: with pkgs; [
    asciinema_3
    alsa-lib
    alsa-utils
    feedbackd
    cudatoolkit
  ];
  graphical = pkgs: with pkgs; [
    godot_4
    gdtoolkit_4

    qemu
    vlc
    libvlc
    xdg-desktop-portal
    gtk3
    qt6Packages.qt6ct
    libsForQt5.qt5.qtgraphicaleffects

    zathura
    texlab
    texliveMedium
    #self.packages.${pkgs.system}.packettracer
    blender
    krita
    drawio
    prismlauncher
    ripgrep
    playerctl
    texlab
    alsa-lib
    xclip scrot grim slurp fuzzel libnotify dunst
    wl-clipboard wf-recorder imagemagick

    blender anki obsidian davinci-resolve orca-slicer

    (heroic.override {
        extraPkgs = pkgs: [
          pkgs.gamescope
        ];
      })
      (vesktop.override {withMiddleClickScroll = true;})
  ];
in {
  flake.nixosModules.packages-common = { pkgs, ...}: {
    environment.systemPackages = common pkgs;
  };
  flake.nixosModules.packages-common-big = { pkgs, ...}: {
    environment.systemPackages = common pkgs ++ big pkgs;
  };
  flake.nixosModules.packages-graphical = { pkgs, ...}: {
    environment.systemPackages = graphical pkgs;
  };
}
