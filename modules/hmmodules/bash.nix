{...}: {
  flake.homeModules.bash = {...}: {
    programs.bash = {
      enable = true;
      bashrcExtra = ''
      export SSH_AUTH_SOCK="/run/user/1000/ssh-agent.socket"
      export FIGNORE=.lock
      if [[ $- == *i* ]]
      then
        fokquote
        fok news -lun5
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
}
