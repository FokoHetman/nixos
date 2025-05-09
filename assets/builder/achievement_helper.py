import os, json
steam_key = "F9EF41084569D7437E3120364F10CA3D"
user_id = 76561199012622408
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


format = f"curl \"https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v1/"
format+= f"?key={steam_key}&steamid={user_id}&language={language}&appid=312520&format=json\""
format+= " -s"
user = json.loads(os.popen(format).read())

achievements = []
for i in user["playerstats"]["achievements"]:
  if i["achieved"]:
    achievements.append((i["apiname"], i["unlocktime"]))

achievements = sorted(achievements, key=lambda x: x[1])

glyphString = ""
for i in achievements:
  glyphString += getGlyph(i[0])

print(glyphString, end='')
