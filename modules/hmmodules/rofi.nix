{lib, ...}: {
  flake.homeModules.rofi = {config, ...}: {
    programs.rofi = {
      enable = true;
      theme = lib.mkForce (let
        inherit (config.lib.formats.rasi) mkLiteral;
        literal0 = mkLiteral "0px";
      in
        {
        "*" = {
          background-color = mkLiteral "transparent";
          text-color = mkLiteral "@fg0";
          margin = literal0;
          padding = literal0;
          spacing = literal0;
          bg0 = mkLiteral "#2E3440F2";
          bg1 = mkLiteral "#3B4252";
          bg2 = mkLiteral "#4C566A80";
          bg3 = mkLiteral "#88C0D0F2";
          fg0 = mkLiteral "#D8DEE9";
          fg1 = mkLiteral "#ECEFF4";
          fg2 = mkLiteral "#D8DEE9";
          fg3 = mkLiteral "#4C566A";
        };
        window = {
          location = mkLiteral "north";
          y-offset = mkLiteral "calc(50% - 176px)";
          width = 480;
          border-radius = mkLiteral "24px";
          background-color = mkLiteral "@bg0";
        };
        mainbox = {
          padding = mkLiteral "12px";
        };
        inputbar = {
          background-color = mkLiteral "@bg1";
          border-color = mkLiteral "@bg3";
          border = mkLiteral "2px";
          border-radius = mkLiteral "16px";
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          children = map mkLiteral [ "prompt" "entry" ];
        };
        prompt.text-color = mkLiteral "@fg2";
        entry = {
          placeholder = mkLiteral "\"Search\"";
          placeholder-color = mkLiteral "@fg3";
        };

        message = {
          margin = mkLiteral "12px 0 0";
          border-radius = mkLiteral "16px";
          border-color = mkLiteral "@bg2";
          background-color = mkLiteral "@bg2";
        };

        textbox.padding = mkLiteral "8px 24px";


        listview = {
          background-color = mkLiteral "transparent";
          margin = mkLiteral "12px 0 0";
          lines = 8;
          columns = 1;
          fixed-height = false;
        };

        element = {
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          border-radius = mkLiteral "16px";


          #normal.active.text-color = mkLiteral "@bg3";
          #alternate.active.text-color = mkLiteral "@bg3";
          #selected.normal.background-color = mkLiteral "@bg3";
          #selected.active.background-color = mkLiteral "@bg3";
        };

        element-icon = {
          size = mkLiteral "1em";
          vertical-align = mkLiteral "0.5";
        };

        element-text.text-color = mkLiteral "inherit";

        #element.selected.text-color = mkLiteral "@bg2";
      });
      extraConfig = {
        #show_icons = true;
      };
    };
  };
}
