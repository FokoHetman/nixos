{
  services.nginx = {
    enable = false;
    /*recommendedProxySettings = true;
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
          proxyPass = "http://git.127.0.0.1:2137";
          proxyWebsockets = true;
        };
      };
    };*/
  };
  /*security.acme = {
    acceptTerms = true;
    defaults.email = "foko@hetman.at";
  };*/
}
