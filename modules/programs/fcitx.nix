{...}: {
  flake.nixosModules.fcitx = {pkgs,...}: {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-gtk kdePackages.fcitx5-qt];
      fcitx5.settings = {
        globalOptions = {
          Hotkey = {
            TriggerKeys = "Ctrl+Alt+space";
          };
        };
        inputMethod = {
          GroupOrder."0" = "Default";
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "pl";
            DefaultIM = "keyboard-pl";
          };
          "Groups/0/Items/0".Name = "keyboard-pl";
          "Groups/0/Items/1".Name = "keyboard-ru";
          "Groups/0/Items/2".Name = "mozc";
        };
      };
    };
  };
}
