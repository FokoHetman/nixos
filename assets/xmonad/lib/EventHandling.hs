module EventHandling where

import Data.Monoid
import XMonad
import XMonad.Hooks.WindowSwallowing
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: (Event -> X All)
myEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True)
