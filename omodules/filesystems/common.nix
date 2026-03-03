{inputs, self, ...}: {
  flake.nixosModules.support-ntfs = { ... }: {
    boot.supportedFilesystems = ["ntfs"];
  };
}
