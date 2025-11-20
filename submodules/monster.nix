{config, options, lib, pkgs, inputs, ...}:
let
  cfg = config.monster;

  enable = lib.mkOption {
    description = "Whether to enable the monster(s).";
    type = lib.types.bool;
    default = false;
  };
  startSize = lib.mkOption {
    description = "Revision from which the size will increase. You can check your current revision using `git rev-list --count --all`";
    type = lib.types.int;
    default = 1;
  };
  maxSize = lib.mkOption {
    description = "The revision after which the monster will not grow.";
    type = lib.types.int;
    default = -1;
  };
  package = lib.mkOption {
    description = "A function that given monster's name and component index, returns a package";
    default = x: let i = toString x.index; in (pkgs.writeShellScriptBin (x.name + "${i}") ''
      echo "This is ${i}th segment of ${x.name}'s body."
    '');
    type = lib.types.functionTo lib.types.package;
  };

  genParts = name: size: f: lib.lists.forEach (lib.lists.range 1 size) (index: f {inherit name index;});
in
{
  options.monster = lib.warnIfNot (inputs.self ? revCount) "You're on a dirty git tree. Monsters will default to size `0`." {
    inherit enable;
    monsters = lib.mkOption {
      description = "Attribute set defining monsters.";
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          inherit startSize maxSize enable package;
        };
      });
      default = {};
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages =
      cfg.monsters |>
      lib.attrsets.filterAttrs (k: v: v.enable) |>
      lib.attrsets.mapAttrsToList (k: v: let size = lib.min (inputs.self.revCount or 0) (v.maxSize - v.startSize); in (genParts k size v.package)) |>
      lib.lists.flatten;
  };
}
