let
  forwardNameserver = x: {"${x}.hetman.at" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://${x}.localhost:2137/";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header Cookie $http_cookie;
          '';
        };
      };};
in
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
          proxyPass = "http://localhost:2137/";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header Cookie $http_cookie;
          '';
        };
        /*locations."/static/" = {
          proxyPass = "http://localhost:2137/";
          extraConfig = ''
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          '';
        };*/
      };
      /*"mail.hetman.at" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://mail.localhost:2137";
          proxyWebsockets = true;
        };
      };*/
    } // forwardNameserver "git" // forwardNameserver "mail" // forwardNameserver "fok";
  };
  security.acme = {
    acceptTerms = true;
    defaults.email = "foko@hetman.at";
  };
}
