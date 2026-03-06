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
    xlayout-sw = pkgs.writeShellScriptBin "xlayout-sw" ''
      case $(setxkbmap -query | grep -oP "(?<=layout:).*" | tr -d [:space:]) in
        pl  ) setxkbmap ru;;
        ru  ) setxkbmap pl;;
        *   ) setxkbmap pl;; # fallback
      esac
      '';
  };
}
