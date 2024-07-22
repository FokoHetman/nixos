{
  inputs,
  lib,
  config,
  pkgs,
  username,
  ...
}: {
  imports = [inputs.ags.homeManagerModules.default];
  nixpkgs = {
    overlays = [];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };
  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
    packages = with pkgs; [
      vesktop
      dolphin
      krita
      prismlauncher

      drawio

      colorls

      mangohud

      colorls
      tree
      gimp

      (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    ];
  };

  fonts.fontconfig.enable = true;

  programs = {
    ags = {
      enable = true;
      configDir = ./ags;
      extraPackages = with pkgs; [
        gtksourceview
        webkitgtk
        accountsservice
      ];
    };

    home-manager.enable = true;
    git.enable = true;
    kitty.enable = true;
    wofi.enable = true;
    firefox = {
      enable = true;
      profiles.yurii = {
        search = {
            force = true;
            default = "DuckDuckGo";
            order = [ "DuckDuckGo" "Google" ];
        };
        #search.privateDefault = "DuckDuckGo";
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

          "Nix Optioms" = {
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
    bash = {
      enable = true;
      bashrcExtra = ''
        export FIGNORE=.lock
        fok-quote
      '';
      historySize = 10000;
      historyControl = ["ignoreboth"];
      enableCompletion=true;
      /*shellAliases = {
        ll = "colorls -l";
        ".." = "cd ..";
        la = "colorls -a";
        lla = "colorls -al";
        ls = "colorls";
      };*/

    };
  };


  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [
      #inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprwinwrap
      #inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprtrails
    ];
  #  package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
  #  systemd.enable = true;
    xwayland.enable = true;

    settings = {

      general = {
        "col.active_border"=lib.mkForce "rgb(${config.stylix.base16Scheme.base0C})";
        "col.inactive_border"=lib.mkForce "rgb(${config.stylix.base16Scheme.base00})";
        border_size = 2;
        layout = "dwindle";
      };
      decoration = {
        rounding = 15;
        active_opacity = 1.0;
        inactive_opacity = 0.98;
        shadow_range = 1;
        shadow_render_power = 1;
        "col.shadow" = lib.mkForce "rgb(${config.stylix.base16Scheme.base0D})";
        shadow_offset = "1 1";

        blur = {
          enabled = true;
          size = 3;
          passes = 1;
          vibrancy = 0.1696;
        };
      };

      plugin = {
        hyprwinwrap = {
          class = "kitty-bg";
        };
      };

      monitor = [
        "Unknown-1,disable"
      ];

      "exec-once" = "ags";

      "$mod" = "SUPER";
      "$browser" = "firefox";
      "$terminal" = "kitty";#"alacritty";
      "$fileManager" = "nemo";
      "$discord" = "vesktop";
      "$menu" = "wofi --show drun --show-icons";

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

        ", Print, exec, grim -g \"$(slurp)\" - | wl-copy"
      ];
    };
  };


  systemd.user.startServices = "sd-switch";
  home.stateVersion = "23.11";
}
