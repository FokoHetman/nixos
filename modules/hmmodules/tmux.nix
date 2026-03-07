{...}: {
  flake.homeModules.tmux = {...}: {
    programs.tmux = {
      enable = true;
      clock24 = true;

      plugins = [

      ];
      extraConfig = ''
        set -g allow-passthrough on
      '';
    };
  };
}
