{...}: {
  flake.homeModules.git = {pkgs, ...}: {
    programs.git = {
      enable = true;
      userName = "FokoHetman";
      userEmail = "foko@hetman.at";
      aliases = {
        c = "commit";
        co = "check-out";
        s = "status";
        p = "pull";
      };
      extraConfig = {
      credential.helper = "${pkgs.git.override { withLibsecret = true; } }/bin/git-credential-libsecret";
      };
    };
  };
}
