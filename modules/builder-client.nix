{...}: {
  flake.nixosModules.builder-client = {...}: {
    nix.buildMachines = [ {
      hostName = "hetman.at:2136";
      protocol = "ssh-ng";
      systems = ["x86_64-linux" "aarch64-linux"];
      maxJobs = 1;
      speedFactor = 5;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    }];
  };
}
