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
