{...}: {
  perSystem.packages = {pkgs,...}: {
    packettracer = pkgs.writeShellScriptBin "packettracer" ''${pkgs.firejail}/bin/firejail --noprofile --net=none ${pkgs.ciscoPacketTracer8}/bin/packettracer8'';
  };
}
