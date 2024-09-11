{pkgs, ...}:
{
  services.minecraft-servers = {
    enable = true;
    eula = true;

    servers = {
      spigottified_friens = {
	enable = true;

	package = pkgs.paperServers.paper-1_21;
	serverProperties = {
	  online-mode = false;
	  motd = "kraby w grach kraby kraby w grach kraby w grach kraby kraby w grach kraby w grach";
	  server-port = 25565;
	};
	symlinks = {
	  #"plugins" = ./minecraft/spigottifiedFriens/plugins;
	};
	jvmOpts = "-Xms2048M -Xmx4092M --XX: +UseG1GC";
      };
    };
  };
}
