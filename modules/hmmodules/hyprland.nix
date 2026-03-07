{inputs, lib, ...}: {
  flake.homeModules.hyprland = {config, pkgs, ...}: {
    wayland.windowManager.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;

      plugins = [
        inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprwinwrap
        #inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprtrails
      ];
      #extraConfig = '' plugin = ${inputs.hyprland-plugins.packages.${pkgs.system}.hyprwinwrap}/lib/libhyprwinwrap.so '';
      #  systemd.enable = true;
      xwayland.enable = true;

      settings = {

        general = {
          "col.active_border"=lib.mkForce "#d65d0e";
          "col.inactive_border"=lib.mkForce "#98971a";
          border_size = 2;
          layout = "dwindle";
        };

        input = {
          "kb_layout" = "pl";
          "kb_variant" = ",qwerty";
        };
        decoration = {
          rounding = 15;
          active_opacity = 1.0;
          inactive_opacity = 0.98;
          /*shadow_range = 1;
shadow_render_power = 1;
"col.shadow" = lib.mkForce "rgb(${config.stylix.base16Scheme.base0D})";
shadow_offset = "1 1";
*/
          blur = {
            new_optimizations = false;
            enabled = true;
            size = 3;
            passes = 1;
            vibrancy = 0.1696;
          };
        };

        plugin = {
          hyprwinwrap = {
            class = "lwpwlp";
          };
        };

        monitor = [
          "Unknown-1,disable"
        ];

        env = [
          "QT_QPA_PLATFORM,wayland"
          "QT_QPA_PLATFORMTHEME,qt6ct"
        ];

        exec-once = [
          "ags"
          "kando"
          "udiskie -c \"$HOME/.config/udiskie/config.yml\""
          "nix run /home/foko/Builds/wallpaper/flake#lwp"
        ];

        "$mod" = "SUPER";
        "$browser" = "librewolf";
        "$terminal" = "kitty";#"alacritty";
        "$fileManager" = "nemo";
        "$discord" = "vesktop";
        "$menu" = "wofi --show drun --show-icons";


        windowrule = [
          "noblur, class:kando,title:Kando"
          "opaque, class:kando,title:Kando"
          "size 100% 100%, class:kando,title:Kando"
          "noborder, class:kando,title:Kando"
          "float, class:kando,title:Kando"
          "pin, class:kando,title:Kando"
          #"pin, class:^.*lwp.*$"
        ];
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
        bind = [
          "$mod, F, exec, $browser"
          "$mod, Q, exec, $terminal"
          "$mod, M, exit,"
          "$mod, V, togglefloating,"

          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, up, movefocus, u"
          "$mod, down, movefocus, d"

          "$mod, C, killactive,"
          "$mod, E, exec, $fileManager"
          "$mod, R, exec, $menu"
          "$mod, T, exec, $discord"
          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, down, movefocus, d"
          "$mod, up, movefocus, u"

          "$mod, 1, workspace, 1"
          "$mod, 2, workspace, 2"
          "$mod, 3, workspace, 3"
          "$mod, 4, workspace, 4"
          "$mod, 5, workspace, 5"
          "$mod, 6, workspace, 6"
          "$mod, 7, workspace, 7"
          "$mod, 8, workspace, 8"
          "$mod, 9, workspace, 9"
          "$mod, 0, workspace, 10"

          "$mod SHIFT, 1, movetoworkspace, 1"
          "$mod SHIFT, 2, movetoworkspace, 2"
          "$mod SHIFT, 3, movetoworkspace, 3"
          "$mod SHIFT, 4, movetoworkspace, 4"
          "$mod SHIFT, 5, movetoworkspace, 5"
          "$mod SHIFT, 6, movetoworkspace, 6"
          "$mod SHIFT, 7, movetoworkspace, 7"
          "$mod SHIFT, 8, movetoworkspace, 8"
          "$mod SHIFT, 9, movetoworkspace, 9"
          "$mod SHIFT, 0, movetoworkspace, 10"
          "ALT, Tab, cyclenext"
          "ALT, Tab, bringactivetotop"
          "$mod, Tab, global, kando:hetmanat"

          ", Print, exec, grim -g \"$(slurp)\" - | wl-copy"

          "$mod, L, exec, hyprlock"
        ];
      };
    };
  };
}
