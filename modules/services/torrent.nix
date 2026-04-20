{inputs, ...}: {
  flake.nixosModules.torrent-server = {config, ...}: {
    services.qbittorrent = {
      enable = true;
      openFirewall = true;
      torrentingPort = 6889;
    };
  };
}
