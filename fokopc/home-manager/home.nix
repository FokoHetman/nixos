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
    overlays = [
      (self: super:
      {
	vencord = super.vencord.overrideAttrs(prev: rec {
	  src = super.fetchFromGitHub {
	    owner = "FokoHetman";
	    repo = "Vencord";
	    rev = "test";
	    hash = "sha256-YyYDHQ03X+//K/wzpwWHgeoyNxGgIPKtkShCikV/1tk=";
	  };
	});
	vesktop = super.vesktop.overrideAttrs(prev: rec {

	  src = super.fetchFromGitHub {
	    owner = "FokoHetman";
	    repo = "Vesktop";
	    rev = "v1.5.3-patched3";
	    hash = "sha256-vWwcFlMtfqOQ2M4NuXLy3C57iTC90gvmq0BeBNFQ3SI=";#"sha256-YyYDHQ03X+//K/wzpwWHgeoyNxGgIPKtkShCikV/1tk=";#"sha256-lFuTTtooc3Gs7GADCvFzM5ZcOu+/3KCN3s0s4Aa09n4=";
	  };
	  #withMiddleClickScroll = true;
	#vesktop.override {withMiddleClickScroll = true;} ;
	});
      })
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };
  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
    packages = with pkgs; [
      obsidian


      (vesktop.override {withMiddleClickScroll = true;/* withSystemVencord = true;*/})
      #vencord
      krita
      prismlauncher

      dolphin
      fastfetch

      drawio
      freecad

      lsd

      mangohud

      tree

      inputs.nixvim.packages.${pkgs.system}.default

      (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    
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

  fonts.fontconfig.enable = true;
  
  xdg.configFile."lf/icons".source = ./icons;
  programs = {
    tmux = {
      enable = true;
      clock24 = true;
      plugins = [

      ];
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
        webkitgtk
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

	#cursor = "#ff8758";
	#cursor_text_color = "#444";
	/*cursor_shape = "beam";
	cursor_beam_thickness = 2;
	cursor_underline_thickness = 2;
	cursor_blink_interval = -1;
	cursor_stop_blinking_after = 15;*/
      };
    };

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
        export FIGNORE=.lock
        fok-quote
      '';
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
      
      input = {
	"kb_layout" = "pl";
	"kb_variant" = ",qwerty";
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

      env = [
        "QT_QPA_PLATFORM,wayland"
        "QT_QPA_PLATFORMTHEME,qt5ct"
      ];

      "exec-once" = "ags";

      "$mod" = "SUPER";
      "$browser" = "firefox";
      "$terminal" = "kitty";#"alacritty";
      "$fileManager" = "dolphin";
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