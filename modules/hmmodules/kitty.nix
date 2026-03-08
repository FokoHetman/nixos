{lib, ...}: {
  flake.homeModules.kitty = {pkgs, ...}: {
    programs.kitty = {
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
      };
    };
  };
}
