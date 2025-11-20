{config, options, lib, pkgs, inputs, ...}:
let
  cfg = config.monster;

  enable = lib.mkOption {
    description = "Whether to enable the monster(s).";
    type = lib.types.bool;
    default = false;
  };
  startSize = lib.mkOption {
    description = "Revision from which the size will increase.";
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
    default = name: i: (pkgs.writeShellScriptBin (name ++ (lib.toString i)) ''
      echo "This is ${i}th segment of ${name}'s body."
    '');
    type = lib.types.functionTo (lib.types.submodule {
      options = {
        name = lib.mkOption { type = lib.types.str; };
        index = lib.mkOption { type = lib.types.int; };
      };
    });
  };

  genParts = name: size: f: lib.lists.forEach (lib.lists.range 1 size) (x: f name x);
in
{
  options.monster = {
    inherit enable;
    monsters = lib.mkOption {
      description = "attribute set defining monsters.";
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
