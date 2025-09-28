{
  inputs,
  lib,
  config,
  pkgs,
  username,
  fonts,
  ...
}: let
  lockscreen = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/FokoHetman/FokWebserver/refs/heads/master/src/static/executing_traitors_liberty_day.png";
    sha256 = "sha256:1580i6mppd8s5fmy5vvhspg58lgfi6qsm7vrh508rpv9piha2556";
  };
  wallpapers = {
    "libertyday.png" = {
      url = "https://raw.githubusercontent.com/FokoHetman/FokWebserver/refs/heads/master/src/static/executing_traitors_liberty_day.png";
      sha256 = "sha256:1580i6mppd8s5fmy5vvhspg58lgfi6qsm7vrh508rpv9piha2556";
    };
  };
  /*builtins.fetchurl { url = "https://fokopi.axolotl-snake.ts.net:10000/static/executing_traitors_liberty_day.png"; 
sha256 = ""; }*/
in{
  imports = [inputs.ags.homeManagerModules.default];
  nixpkgs = {
    overlays = [
      (self: super:
      {
      })
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
      permittedInsecurePackages = [
        "libsoup-2.74.3"
      ];
    };
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
    };
  };


  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
    #fonts.packages = with pkgs; [
    #  nerd-fonts.fira-code
    #  nerd-fonts.droid-sans-mono
    #];
    packages = with pkgs; [
      #jetbrains.idea-community #pls install pluginss here
      #(jetbrains.plugins.addPlugins jetbrains.idea-community ["minecraft-dev"])
      obsidian

      inputs.quickshell.packages.${system}.default

      davinci-resolve

      kando

      #xmobar
      #haskellPackages.xmobar
      
      orca-slicer

      (writeShellApplication {
        name = "ns";
        runtimeInputs = with pkgs; [
          fzf
          nix-search-tv
        ];
        # ignore checks since i didn't write this
        checkPhase = "";
        text = builtins.readFile "${pkgs.nix-search-tv.src}/nixpkgs.sh";
      })


      (inputs.blackmarket.legacyPackages.${pkgs.system}.lwp)
      xorg.xhost


      hashcat


      (vesktop.override {withMiddleClickScroll = true;/* withSystemVencord = true;*/})
      krita
      prismlauncher

      nemo
      fastfetch

      drawio
      lsd

      tree

      godot_4

      #inputs.nixvim.packages.${pkgs.system}.default

      #(nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
      nerd-fonts.fira-code
      nerd-fonts.droid-sans-mono
      (fonts.rainworld)

      (pkgs.writeShellScriptBin "xmobar-voice-control" /*bash*/ ''
        case "$1" in
          1) amixer set Master toggle ;;
          4) amixer set Master $2%+   ;;
          5) amixer set Master $2%-   ;;
        esac
      '')
    (pkgs.writeShellScriptBin "recorder" /*bash*/ ''
      #! /usr/bin/env nix-shell
      #! nix-shell -i bash -p bash

      wf-recorder_check() {
	if pgrep -x "wf-recorder" > /dev/null; then
			pkill -INT -x wf-recorder
			notify-send "Stopping all instances of wf-recorder" "$(cat /tmp/recording.txt)"
			wl-copy < "$(cat /tmp/recording.txt)"
			exit 0
	fi
      }

      wf-recorder_check

      SELECTION=$(echo -e "screenshot selection\nscreenshot DP-1\nscreenshot DP-2\nscreenshot both screens\nrecord selection\nrecord DP-1\nrecord DP-2" | fuzzel -d -p "󰄀 " -w 25 -l 6)

      IMG="/home/${username}/Media/Screenshots/$(date +%Y-%m-%d_%H-%m-%s).png"
      VID="/home/${username}/Media/Recordings/$(date +%Y-%m-%d_%H-%m-%s).mp4"


      case "$SELECTION" in
	"screenshot selection")
		grim -g "$(slurp)" "$IMG"
		wl-copy < "$IMG"
		notify-send "Screenshot Taken" "$\{IMG}"
		;;
	"screenshot DP-1")
		grim -c -o DP-1 "$IMG"
		wl-copy < "$IMG"
		notify-send "Screenshot Taken" "$\{IMG}"
		;;
	"screenshot DP-2")
		grim -c -o DP-2 "$IMG"
		wl-copy < "$IMG"
		notify-send "Screenshot Taken" "$\{IMG}"
		;;
	"screenshot both screens")
		grim -c -o DP-1 "$\{IMG//.png/-DP-1.png}"
		grim -c -o DP-2 "$\{IMG//.png/-DP-2.png}"
		montage "$\{IMG//.png/-DP-1.png}" "$\{IMG//.png/-DP-2.png}" -tile 2x1 -geometry +0+0 "$IMG" 
		wl-copy < "$IMG"
		rm "$\{IMG//.png/-DP-1.png}" "$\{IMG/.png/-DP-2.png}"
		notify-send "Screenshot Taken" "$\{IMG}"
		;;
	"record selection")
		echo "$VID" > /tmp/recording.txt
		wf-recorder -a -g "$(slurp)" -f "$VID" &>/dev/null
		;;
	"record DP-1")
		echo "$VID" > /tmp/recording.txt
		wf-recorder -a -o DP-1 -f "$VID" &>/dev/null
		;;
	"record DP-2")
		echo "$VID" > /tmp/recording.txt
	wf-recorder -a -o DP-2 -f "$VID" &>/dev/null
	;;
	"record both screens")
	  notify-send "recording both screens is not functional"
	;;
	*)
	;;
    esac
    '')


    ];
  };

  gtk.enable = true;
  qt.enable = true;
  

  fonts.fontconfig.enable = true;
  
  xdg.configFile."lf/icons".source = ./icons;
  programs = {
    /*xmobar = {
      enable = true;
      extraConfig = lib.readFile ../../assets/xmobar/xmobar.hs;
    };*/
    hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = true;
          #grace = 300;
          hide_cursor = false;
        };

        background = lib.mkOverride 1 [
          {
            path = lockscreen;
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

    emacs = {
      enable = true;
      package = pkgs.emacs;
      extraConfig = ''
	(setq standard-indent 2)
      '';
    };
    tmux = {
      enable = true;
      clock24 = true;

      plugins = [

      ];
      extraConfig = ''
        set -g allow-passthrough on
      '';
    };
    lf = {
      enable = true;
      settings = {
        preview = true;
        drawbox = true;
        icons = true;
      };
      commands = {
        ripdrag = ''%${pkgs.ripdrag}/bin/ripdrag -x "$fx"'';
        edit = ''$$EDITOR $f'';
        mkdir = ''
        ''${{
          printf "Directory Name: "
          read DIR
          mkdir $DIR
        }}'';
        shell = ''
        ''${{
          printf("$: ")
          read COMMAND
          $COMMAND
          read NULL
        }}'';

	compile = ''
	''${{
	  set -m
	  extension=$(echo "$fx" | cut -d "." -f 2)
	  fxnoext=$(echo "fx" | cut -d "." -f 1)
	  case "$extension" in
	    rs		) ${pkgs.rustc}/bin/rustc $fx;;
	    c		) ${pkgs.gcc}/bin/gcc -o $fxnoext $fx;;
	    zig		) ${pkgs.zig}/bin/zig $fx;;
	    hs		) ${pkgs.ghc}/bin/ghc $fx;;
	    py		) ${pkgs.python3}/bin/python $fx;;
	    *		) echo "Unknown extension";;
	  esac

	}}'';
	execute = ''
	''${{
	  ${pkgs.bash}/bin/bash -c $fx
	}}'';


	fok-utils = ''
	''${{
	  fok-utils
	}}'';

        quit = "q";
      };
      keybindings = {
        "\\\"" = "";
        "o" = "";
        "c" = "shell";
	"b" = "compile";
	"x" = "execute";
        "." = "set hidden!";        

        "<enter>" = "open";
        "<c-c>" = "quit";
        "<esc>" = "quit";
        "e" = "edit";
        "f" = "fok-utils";
        "d" = "ripdrag";


        "V" = ''''$${pkgs.bat}/bin/bat --paging=always --theme=gruvbox "$f"'';
      };
      /*previewer = {
        keybinding = "i";
        source = "${pkgs.ctpv}/bin/ctpv";
      };*/
      extraConfig = 
      let 
      previewer = pkgs.writeShellScriptBin "pv.sh" ''
        file=$1
        w=$2
        h=$3
        x=$4
        y=$5
          
        if [[ "$( ${pkgs.file}/bin/file -Lb --mime-type "$file")" =~ ^image ]]; then
            ${pkgs.kitty}/bin/kitty +kitten icat --silent --stdin no --transfer-mode file --place "''${w}x''${h}@''${x}x''${y}" "$file" < /dev/null > /dev/tty
            exit 1
        fi
        
        ${pkgs.pistol}/bin/pistol "$file"
      '';
      cleaner = pkgs.writeShellScriptBin "clean.sh" ''
        ${pkgs.kitty}/bin/kitty +kitten icat --clear --stdin no --silent --transfer-mode file < /dev/null > /dev/tty
      '';
      in
      ''
        set cleaner ${cleaner}/bin/clean.sh
        set previewer ${previewer}/bin/pv.sh
      '';
    };
    ags = {
      enable = true;
      configDir = ./ags;
      extraPackages = with pkgs; [
        gtksourceview
        webkitgtk_6_0
        accountsservice
      ];
    };

    home-manager.enable = true;
    git = {
      enable = true;
      userName = "FokoHetman";
      userEmail = "paprykkania@gmail.com";
      aliases = {
        c = "commit";
        co = "check-out";
        s = "status";
        p = "pull";
      };
      extraConfig = {
      credential.helper = "${pkgs.git.override { withLibsecret = true; } }/bin/git-credential-libsecret";
      };
    };
    kitty = {
      enable = true;
      font = lib.mkForce {
        name = "FiraCode Nerd Font Reg";
        size = 12;
      };
      settings = {
        confirm_os_window_close = 0;
        tab_bar_min_tabs = 1;
        tab_bar_edge = "bottom";
        tab_bar_style = "powerline";
        tab_powerline_style = "slanted";
        tab_title_template = "{title}{' :{}:'.format(num_windows) if num_windows > 1 else ''}";

        symbol_map = let
          mappings = [
            "U+E100-U+E160"
          ];
        in
          (builtins.concatStringsSep "," mappings) + " RainWorldSymbols";
        #cursor = "#ff8758";
        #cursor_text_color = "#444";
        /*cursor_shape = "beam";
        cursor_beam_thickness = 2;
        cursor_underline_thickness = 2;
        cursor_blink_interval = -1;
        cursor_stop_blinking_after = 15;*/
      };
    };
    rofi = {
      enable = true;
      theme = lib.mkForce (let
        inherit (config.lib.formats.rasi) mkLiteral;
        literal0 = mkLiteral "0px";
      in
      {
        "*" = {
          background-color = mkLiteral "transparent";
          text-color = mkLiteral "@fg0";
          margin = literal0;
          padding = literal0;
          spacing = literal0;
          bg0 = mkLiteral "#2E3440F2";
          bg1 = mkLiteral "#3B4252";
          bg2 = mkLiteral "#4C566A80";
          bg3 = mkLiteral "#88C0D0F2";
          fg0 = mkLiteral "#D8DEE9";
          fg1 = mkLiteral "#ECEFF4";
          fg2 = mkLiteral "#D8DEE9";
          fg3 = mkLiteral "#4C566A";
        };
        window = {
          location = mkLiteral "north";
          y-offset = mkLiteral "calc(50% - 176px)";
          width = 480;
          border-radius = mkLiteral "24px";
          background-color = mkLiteral "@bg0";
        };
        mainbox = {
          padding = mkLiteral "12px";
        };
        inputbar = {
          background-color = mkLiteral "@bg1";
          border-color = mkLiteral "@bg3";
          border = mkLiteral "2px";
          border-radius = mkLiteral "16px";
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          children = map mkLiteral [ "prompt" "entry" ];
        };
        prompt.text-color = mkLiteral "@fg2";
        entry = {
          placeholder = mkLiteral "\"Search\"";
          placeholder-color = mkLiteral "@fg3";
        };

        message = {
          margin = mkLiteral "12px 0 0";
          border-radius = mkLiteral "16px";
          border-color = mkLiteral "@bg2";
          background-color = mkLiteral "@bg2";
        };

        textbox.padding = mkLiteral "8px 24px";
        

        listview = {
          background-color = mkLiteral "transparent";
          margin = mkLiteral "12px 0 0";
          lines = 8;
          columns = 1;
          fixed-height = false;
        };

        element = {
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          border-radius = mkLiteral "16px";
        

          #normal.active.text-color = mkLiteral "@bg3";
          #alternate.active.text-color = mkLiteral "@bg3";
          #selected.normal.background-color = mkLiteral "@bg3";
          #selected.active.background-color = mkLiteral "@bg3";
        };

        element-icon = {
          size = mkLiteral "1em";
          vertical-align = mkLiteral "0.5";
        };

        element-text.text-color = mkLiteral "inherit";

        #element.selected.text-color = mkLiteral "@bg2";
      });
      extraConfig = {
        #show_icons = true;
      };
    };
    wofi = {
      enable = true;
      settings = {
        allow_markup = true;
        allow_images = true;
      };
    };
    /*firefox = { */librewolf = {
      enable = true;
      profiles.yurii = {
	containers.school = {
          color = "red";
          icon = "fruit";
        };
        extensions = with (inputs.nur.overlay pkgs pkgs).nur.repos.rycee.firefox-addons; [
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
    bash = {
      enable = true;
      bashrcExtra = ''
      export SSH_AUTH_SOCK="/run/user/1000/ssh-agent.socket"
      export FIGNORE=.lock
	    if [[ $- == *i* ]]
      then
        fok-quote
        fok news -lun5
      fi
      '';
/*
	_nixos()
	{
	  local cur=${COMP_WORDS[COMP_CWORD]}
	  COMPREPLY=( $(compgen -W "sw test edit up" -- $cur) )
	}
	complete -F _nixos nixos
        fok-quote
*/
      historySize = 10000;
      historyControl = ["ignoreboth"];
      enableCompletion=true;
      shellAliases = {
        ll = "lsd -l";
        ".." = "cd ..";
        la = "lsd -a";
        lla = "lsd -al";
        ls = "lsd";
	      tree = "lsd --tree";
      };

    };
  };

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
        "col.active_border"=lib.mkForce "rgb(${config.stylix.base16Scheme.base0C})";
        "col.inactive_border"=lib.mkForce "rgb(${config.stylix.base16Scheme.base00})";
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
        "QT_QPA_PLATFORMTHEME,qt5ct"
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
  
  home.sessionVariables = {
    STEAM_EXTRA_COMPAT_TOOLS_PATHS =
      "\\\${HOME}/.steam/root/compatibilitytools.d";
  };

  home.file = let 
    wallpaperFiles = lib.attrsets.mapAttrs' 
      (name: value: lib.attrsets.nameValuePair (".config/wallpapers/def/" + name) {source = pkgs.fetchurl { inherit (value) url sha256;};}) 
      wallpapers;
  in {
    ".config/neofetch/config.conf" = {
      source = ./dotfiles/neofetch.conf;
    };
    ".config/udiskie/config.yml" = {
      source = ./dotfiles/udiskie.yml;
    };
    ".config/lwp/wallpapers" = {
      source = ./dotfiles/lwp/wallpapers;
    };
    ".ghci" = {
      text = ''
        :set prompt "\955> "
      '';
    };
  } // wallpaperFiles;


  systemd.user.startServices = "sd-switch";
  home.stateVersion = "23.11";
}
