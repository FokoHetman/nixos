{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages.ffmpegxcb-sw = pkgs.ffmpeg.override {withXcb = true;};
  };
}
