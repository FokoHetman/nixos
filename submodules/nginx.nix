{
  networking.upnp.nginx = {
    description = "Nginx port mapping";
    ports = [80 443];
    ignore = true;
    bindsTo = "nginx.service";
  };
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "hetman.at" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:2137";
          proxyWebsockets = true;
        };
      };
      "mail.hetman.at" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://mail.localhost:2137";
          proxyWebsockets = true;
        };
      };
      "git.hetman.at" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://git.localhost:2137";
          proxyWebsockets = true;
        };
      };
    };
  };
  security.acme = {
    acceptTerms = true;
    defaults.email = "foko@hetman.at";
  };
}
