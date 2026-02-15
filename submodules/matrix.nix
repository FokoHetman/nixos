{pkgs, config, ...}:
{
  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = "https://ntfy.hetman.at";
      listen-http = "127.0.0.1:6168";
      behind-proxy = true;
    };
  };
  services.matrix-conduit = {
    enable = true;
    settings.global = {
      #allow_registration = true;
      #registration_token = "fokker"; # this is really horrifying ngl
      server_name = "hetman.at";
      port = 6167;
      address = "127.0.0.1";
      database_backend = "rocksdb";
    };
  };
  networking.firewall.allowedTCPPorts = [6167];
  services.nginx.virtualHosts."hetman.at".locations."/.well-known/matrix/client" = {
    extraConfig = ''
      default_type application/json;
      return 200 '{"m.homeserver": {"base_url": "https://matrix.hetman.at"}}';
    '';
  };

  services.nginx.virtualHosts."hetman.at".locations."/.well-known/matrix/server" = {
    extraConfig = ''
      default_type application/json;
      return 200 '{"m.server": "matrix.hetman.at:443"}';
    '';
  };
  services.nginx.virtualHosts."matrix.hetman.at" = {
    addSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:6167/";
      proxyWebsockets = true;
    };
  };
  services.nginx.virtualHosts."ntfy.hetman.at" = {
    addSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:6168/";
      proxyWebsockets = true;
    };
    extraConfig = ''
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Cookie $http_cookie;
    '';
  };
}
