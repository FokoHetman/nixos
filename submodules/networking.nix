{
  networking.wireless.userControlled.enable=true;
  networking.wireless.networks = {
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
}
