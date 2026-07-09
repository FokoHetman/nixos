{...}: {
  flake.nixosModules.default-networking = {...}: {
    networking = {
      dhcpcd.wait = "background";
      wireless = {
        enable = true;
        userControlled.enable=true;
        networks = {
          Battle_Droid_B1 = {
            priority=1;
            pskRaw="c67893ba49b1f431999823969fa3e70c1242144a0827fbda8048f0a1d7c1c416";
          };
          "c4807a-2.4G" = {
            priority=2;
            pskRaw="4345e96283144ceb369d10773d892e23587f761459ea32f348e2ba9488a02f57";
          };
          FokNet = {
            priority=3;
            auth = ''
        key_mgmt=SAE
        sae_password="hetmanfoko"
  ieee80211w=2
            '';
            #pskRaw="5a16b932a74faab9968a5cdcf7603a6a562ccbaf523f6a3d036cd904996b2159";
          };
        };
      };
      firewall.enable = true;
      #firewall.allowedTCPPorts = [22 25 44 80 443 2137 2138 5900 5901 8000 8080 25565 51413];
      #firewall.allowedUDPPorts = [5900 5901 25565 51413];
      hosts = {
        "127.0.0.1" = [
          "chatgpt.com" "www.chatgpt.com" "openai.com" "www.openai.com" "api.openai.com" "chat.openai.com" "platform.openai.com"
          "gemini.google.com" "bard.google.com" "ai.google.com" "makersuite.google.com"
          "anthropic.com" "www.anthropic.com" "claude.ai" "www.claude.ai" "api.anthropic.com"
          "perplexity.ai" "www.perplexity.ai" "labs.perplexity.ai" "api.perplexity.ai"
          "meta.ai" "www.meta.ai" "ai.facebook.com"
          "mistral.ai" "www.mistral.ai" "api.mistral.ai"
          "x.ai" "www.x.ai" "grok.x.ai" "api.x.ai"
          "cohere.com" "www.cohere.com" "api.cohere.ai"
          "cloud.ibm.com" "watson.ibm.com"
          "cke.gov.pl"
          
          "paradise-s1.battleye.com" "test-s1.battleye.com" "paradiseenhanced-s1.battleye.com"
        ];
      };
    };
  };
}
