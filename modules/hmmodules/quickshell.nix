{inputs, ...}: {
  flake.homeModules.quickshell = {pkgs, ...}: {
    programs.quickshell = {
      enable = true;
      package = inputs.quickshell.packages.${pkgs.system}.default;
      configs = {
        "control-panel" = ./dots/quickshell/panel;
        "notifications" = ./dots/quickshell/notif;
      };
      activeConfig = "control-panel";
    };
  };
}
