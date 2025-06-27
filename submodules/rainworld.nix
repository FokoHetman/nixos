{ config, options, lib, pkgs, ... }:
with lib;  
let
  cfg = config.rainworld;
  
  generateGlyphs = pkgs.writers.writePython3Bin "generate_glyphs" {
    libraries = with pkgs.python3Packages; [ requests ];
    flakeIgnore = ["E111" "E114" "E121" "E221" "E251" "E127" "E128" "E201" "E202" "W291" "W293" "W391" "E265" "E302" "E303" "E305" "E501" "E231" "E261" "E225" "E401"];
  } /*python3*/ ''
import requests
f = open("${cfg.steamkey_path}", "r")
steam_key = f.read()
f.close()
f = open("${cfg.steamid_path}", "r")
user_id = f.read()
f.close()
language = "en"


def getGlyph(s):
  match s:
    case "PassageSurvivor":
      return ""
    case "PassageHunter":
      return ""
    case "PassageMonk":
      return ""
    case "PassageSaint":
      return ""
    case "PassageOutlaw":
      return ""
    case "PassageDragonSlayer":
      return ""
    case "PassageChieftain":
      return ""
    case "PassageTraveller":
      return ""
    case "PassageScholar":
      return ""
    case "PassageFriend":
      return ""
    case "AllGhostsEncountered":
      return ""
    case "MoonEncounterBad":
      return ""
    case "MoonEncounterGood":
      return ""
    case "PebblesEncounter":
      return ""
    case "Win":
      return ""
    case "HunterPayload":
      return ""
    case "HunterWin":
      return ""
    case "GourmandEnding":
      return ""
    case "ArtificerEnding":
      return ""
    case "RivuletEnding":
      return ""
    case "SpearmasterEnding":
      return ""
    case "SaintEnding":
      return ""
    case "ChallengeMode":
      return ""
    case "Quests":
      return ""
    case "PassageMartyr":
      return ""
    case "PassageNomad":
      return ""
    case "PassagePilgrim":
      return ""
    case "PassageMother":
      return ""
    case _:
      if "Ghost" in s:
        return ""


#format = "curl \"https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v1/"
#format+= f"?key={steam_key}&steamid={user_id}&language={language}&appid=312520&format=json\""
#format+= " -s"
#user = json.loads(os.popen(format).read())

user = requests.get(f"https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v1/?key={steam_key}&steamid={user_id}&language={language}&appid=312520&format=json").json()

achievements = []
for i in user["playerstats"]["achievements"]:
  if i["achieved"]:
    achievements.append((i["apiname"], i["unlocktime"]))

achievements = sorted(achievements, key=lambda x: x[1])

glyphString = ""
for i in achievements:
  glyphString += getGlyph(i[0])

print(glyphString, end="")

'';

  
in
{
  options = {
    rainworld = {
      enable = mkOption {
        description = "Whether to enable this module.";
        type = types.bool;
        default = false;
      };
      steamkey_path = mkOption {
        description = "Path to your Steam API Key.";
        type = types.str;
        default = "";
      };
      steamid_path = mkOption {
        description = "Path to your Steam User ID.";
        type = types.str;
        default = "";
      };
    };
  };
  config = mkIf cfg.enable {
    # YOU LAZY DUMBFOK REWRITE IT AS STRING CONCATS!!!! (probably very hard but shut up)
    system.activationScripts."rainworld_glyphs" = ''
      GLYPHS=$(${generateGlyphs}/bin/generate_glyphs)
      [ -n "$GLYPHS" ] && echo $GLYPHS > /tmp/glyphs.txt
    '';
  };
}
