{
  inputs,
  lib,
  config,
  pkgs,
  username,
  fonts,
  ...
}: {
  imports = [inputs.ags.homeManagerModules.default];
  nixpkgs = {
    overlays = [
      (self: super:
      {
	/*vencord = super.vencord.overrideAttrs(prev: rec {
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
	});*/
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
    #fonts.packages = with pkgs; [
    #  nerd-fonts.fira-code
    #  nerd-fonts.droid-sans-mono
    #];
    packages = with pkgs; [
      #jetbrains.idea-community #pls install pluginss here
      #(jetbrains.plugins.addPlugins jetbrains.idea-community ["minecraft-dev"])

      hashcat

      fastfetch

      lsd

      tree

      #inputs.nixvim.packages.${pkgs.system}.default

      #(nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
      nerd-fonts.fira-code
      nerd-fonts.droid-sans-mono
      (fonts.rainworld)
    ];
  };
  fonts.fontconfig.enable = true;
  
  xdg.configFile."lf/icons".source = ./icons;
  programs = {
    

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
      safe.directory = "/home/git/*";
      credential.helper = "${pkgs.git.override { withLibsecret = true; } }/bin/git-credential-libsecret";
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
  

  home.file = {
    ".ghci" = {
      text = ''
        :set prompt "\955> "
      '';
    };
  };


  systemd.user.startServices = "sd-switch";
  home.stateVersion = "23.11";
}
