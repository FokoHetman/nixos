{inputs, ...}: {
  flake.homeModules.browser = {pkgs, ...}: {
    programs = rec {
      qutebrowser = {
        enable = true;
      };
      librewolf = {
        enable = true;
        profiles.foko = {
          containers.school = {
            color = "red";
            icon = "fruit";
          };
          settings = {
            "general.useragent.override" = "Gleep Glorp Browser (Dos) 1921 (KHTML, like Gecko) Chrome/Mosin/Nagant/136.6";
          };
          extensions = with (inputs.nur.overlays.default pkgs pkgs).nur.repos.rycee.firefox-addons; [
            firefox-color
            sidebery
            sponsorblock
            stylus
            tampermonkey
            ublock-origin
            # wakatime
            vimium
          ];

          search = {
            force = true;
            default = "ddg";
            order = [ "ddg" "google" ];
          };
          search.engines = {
            "Jisho" = {
              urls = [{
                template = "https://jisho.org/search/{searchTerms}";
              }];
              icon="https://jisho.org/favicon.ico";
              definedAliases = [ "@ji" "@ja" ];
            };
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "type"; value = "packages";}
                  { name = "query"; value = "{searchTerms}";}
                ];
              }];
              icon="${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };

            "Nix Options" = {
              urls = [{
                template = "https://search.nixos.org/options";
                params = [
                  { name = "type"; value = "packages";}
                  { name = "query"; value = "{searchTerms}";}
                  { name = "channel"; value = "unstable";}
                ];
              }];
              icon="${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            };
          };
        };
      };
      firefox = librewolf;
    };
  };
}
