{lib, self, ...}: {
  flake.homeModules.hyprlock = {...}: {
    programs.hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = true;
          #grace = 300;
          hide_cursor = false;
        };

        background = lib.mkOverride 1 [
          {
            path = builtins.fetchurl (self.globals.wallpapers."libertyday.png");
            blur_passes = 2;
            blur_size = 7;
          }
        ];
        label = [
          # DATE
          {
            monitor = "";
            text = ''cmd[update:1000] echo "$(date +"%A, %B %d")"'';
            color = "rgba(242, 243, 244, 0.75)";
            font_size = 22;
            position = "0, 300";
            halign = "center";
            valign = "center";
          }

          # TIME
          {
            monitor = "";
            text = ''cmd[update:1000] echo "$(date +"%-H:%M")"'';
            color = "rgba(242, 243, 244, 0.75)";
            font_size = "95";
            position = "0, 200";
            halign = "center";
            valign = "center";
          }
        ];

        input-field = lib.mkForce [
          {
            size = "250, 50";
            position = "0, -80";
            monitor = "";
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            placeholder_text = "<span foreground=\"##cad3f5\">Password...</span>";
            shadow_passes = 2;
          }
        ];
        auth.pam = {
          enabled = true;
          module = "su";
        };
      };
    };
  };
}
