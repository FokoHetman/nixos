{config, lib, pkgs, input, ...}:
  let
  sshdTmpDirectory = "${config.user.home}/sshd-tmp";
  sshdDirectory = "${config.user.home}/sshd";
  pathToPubKey = "${config.user.home}/keys/id_rsa.pub";
  port = 8022;
in
{
  build.activation.sshd = ''
    $DRY_RUN_CMD mkdir $VERBOSE_ARG --parents "${config.user.home}/.ssh"
    $DRY_RUN_CMD cat ${pathToPubKey} > "${config.user.home}/.ssh/authorized_keys"

    if [[ ! -d "${sshdDirectory}" ]]; then
      $DRY_RUN_CMD rm $VERBOSE_ARG --recursive --force "${sshdTmpDirectory}"
      $DRY_RUN_CMD mkdir $VERBOSE_ARG --parents "${sshdTmpDirectory}"

      $VERBOSE_ECHO "Generating host keys..."
      $DRY_RUN_CMD ${pkgs.openssh}/bin/ssh-keygen -t rsa -b 4096 -f "${sshdTmpDirectory}/ssh_host_rsa_key" -N ""

      $VERBOSE_ECHO "Writing sshd_config..."
      $DRY_RUN_CMD echo -e "HostKey ${sshdDirectory}/ssh_host_rsa_key\nPort ${toString port}\n" > "${sshdTmpDirectory}/sshd_config"

      $DRY_RUN_CMD mv $VERBOSE_ARG "${sshdTmpDirectory}" "${sshdDirectory}"
    fi
  '';
  nix.extraOptions = ''
          experimental-features = ${
            builtins.concatStringsSep " " [
              "nix-command"
              "flakes"
              "recursive-nix"
            ]
          }
    builders = ${
      # TODO: <https://nix.dev/manual/nix/2.18/advanced-topics/distributed-builds>
      builtins.concatStringsSep " ; " [
        "ssh-ng://hetman.at:2136               x86_64-linux,aarch64-linux - 16 6 benchmark,big-parallel,kvm,nixos-test -"
      ]
    }
      builders-use-substitutes = true
      warn-dirty = false
  '';
  terminal.colors = {
    cursor = "#FFFFFF";
    background = "#282828";
    foreground =  "#EBDBB2";
  };

}
