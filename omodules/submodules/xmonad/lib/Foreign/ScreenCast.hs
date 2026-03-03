module Foreign.ScreenCast where
-- https://www.reddit.com/r/xmonad/comments/j5419h/gif_screen_capture/

import qualified XMonad.Util.ExtensibleState as XS
import XMonad

newtype Screencast = Screencast Bool deriving (Read, Show, Typeable)

instance ExtensionClass Screencast where
  initialValue = Screencast False
  extensionType = PersistentExtension

screencast :: [String] -> X ()
screencast args = do
  Screencast on <- XS.get
  spawn $
    if on
      then "killall ffmpeg"
      else unwords ("screencast" : args)
  XS.put $ Screencast (not on)
