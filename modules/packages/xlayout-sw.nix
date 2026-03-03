{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages.xlayout-sw = pkgs.writeShellScriptBin "xlayout-sw" ''
      case $(setxkbmap -query | grep -oP "(?<=layout:).*" | tr -d [:space:]) in
        pl  ) setxkbmap ru;;
        ru  ) setxkbmap pl;;
        *   ) setxkbmap pl;; # fallback
      esac
      '';
  };
}
