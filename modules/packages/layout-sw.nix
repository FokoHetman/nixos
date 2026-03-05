{inputs, ...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    layout-sw = pkgs.writeShellScriptBin "layout-sw" ''
      case $(fcitx5-remote -n) in
        keyboard-pl   ) fcitx5-remote -s keyboard-ru;;
        keyboard-ru   ) fcitx5-remote -s mozc;;
        mozx          ) fcitx5-remote -s keyboard-pl;;
        *             ) fcitx5-remote -s keyboard-pl;; # fallback
      esac
      '';
  };
}
