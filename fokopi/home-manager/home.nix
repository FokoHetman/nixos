{
  inputs,
  lib,
  config,
  pkgs,
  username,
  ...
}: {
  imports = [];
  nixpkgs = {
    overlays = [
      (self: super:
      {

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
      (pkgs.writeShellScriptBin "wakethefokup" ''
	${pkgs.wakeonlan}/bin/wakeonlan -i 169.254.255.255 74:56:3c:1b:d0:90
      '')

      fastfetch

      drawio
      freecad

      lsd

      mangohud

      tree

      inputs.nixvim.packages.${pkgs.system}.default

      (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    ];
  };

  fonts.fontconfig.enable = true;
  
  xdg.configFile."lf/icons".source = ./icons;
  programs = {
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
      previewer = {
        keybinding = "i";
        source = "${pkgs.ctpv}/bin/ctpv";
      };
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
      };

    };
  };


  systemd.user.startServices = "sd-switch";
  home.stateVersion = "23.11";
}
