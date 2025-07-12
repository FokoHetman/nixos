{
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
          extraConfig = ''
            proxy_set_header = X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          '';
        };
      };
      /*"mail.hetman.at" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://mail.localhost:2137";
          proxyWebsockets = true;
        };
      };*/
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
