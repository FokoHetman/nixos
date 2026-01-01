module Vimization where
import XMonad
import Data.IORef (IORef, writeIORef, readIORef)
import XMonad.Util.EZConfig (additionalKeys)
import Control.Arrow (Arrow(second))
import Definitions
import XMonad.Layout.WindowArranger
import qualified XMonad.StackSet as W

data Mode = Normal | Prompt | Resize | Move | Focus | Launch | Media deriving (Eq, Show)

switchMode :: IORef Mode -> Mode -> X ()
switchMode modeRef newMode = do
  liftIO $ writeIORef modeRef newMode
  refresh

modeMod :: KeyMask
modeMod = mod1Mask

modeKeys :: IORef Mode -> XConfig a -> XConfig a
modeKeys ref = (`additionalKeys` fmap (second (switchMode ref)) [
    ((modeMod, xK_Escape), Normal)
  , ((modeMod, xK_r), Resize)
  , ((modeMod, xK_f), Focus)
  , ((modeMod, xK_w), Move)
  , ((modeMod, xK_p), Prompt)
  , ((modeMod, xK_m), Media)
  ])

dynamicKeys :: IORef Mode -> [((KeyMask, KeySym), X ())]
dynamicKeys modeRef = [
    ((modeMod, xK_h), modeDispatch modeRef 
    [
      (Normal, pure ())
    , (Prompt, hooglePrompt myXPConfig "hoogle")
    , (Resize, sendMessage Shrink)
    , (Move  , sendMessage (MoveLeft 10))
    , (Focus , pure ())
    , (Launch, pure ())
    ])
  , ((modeMod, xK_j), modeDispatch modeRef
    [
      (Normal, pure ())
    , (Prompt, pure ())
    , (Resize, pure ())
    , (Move  , sendMessage (MoveDown 10))
    , (Focus , windows W.focusDown)
    , (Launch, pure ())
    ])
  , ((modeMod, xK_k), modeDispatch modeRef
    [
      (Normal, pure ())
    , (Prompt, pure ())
    , (Resize, pure ())
    , (Move  , sendMessage (MoveUp 10))
    , (Focus , windows W.focusUp)
    , (Launch, pure ())
    ])
  , ((modeMod, xK_l), modeDispatch modeRef
    [
      (Normal, pure ())
    , (Prompt, pure ())
    , (Resize, sendMessage Expand)
    , (Move  , sendMessage (MoveRight 10))
    , (Focus , pure ())
    , (Launch, pure ())
    ])
  , ((modeMod, xK_n), modeDispatch modeRef
    [
      (Normal, pure ())
    , (Prompt, nooglePrompt myXPConfig "noogle")
    , (Resize, pure ())
    , (Move  , pure ())
    , (Focus , pure ())
    , (Launch, pure ())
    ])
  , ((modeMod, xK_space), modeDispatch modeRef
    [
      (Media, spawn "playerctl play-pause")
    ])
  ]

modeDispatch :: IORef Mode -> [(Mode, X ())] -> X ()
modeDispatch ref links = do
  mode <- liftIO $ readIORef ref
  case lookup mode links of
    Just x  -> x
    Nothing -> pure ()

vimize :: IORef Mode -> XConfig a -> XConfig a
vimize ref = modeKeys ref . (`additionalKeys` dynamicKeys ref)



wrapColor :: String -> String -> String -> String
wrapColor bg fg txt = "<box type=Full fill=true width=2 mb=0 color=" ++ bg ++ "><fc=" ++ fg ++ "," ++ bg ++ ":0>" ++ txt ++ "</fc></box>"

modeLogger :: IORef Mode -> X (Maybe String)
modeLogger modeRef = do
  mode <- liftIO $ readIORef modeRef
  let icon txt = "<fn=2>" ++ txt ++ "</fn>"
  let formatMode bg fg name = wrapColor bg fg (icon " \xE61F " ++ name)
  pure . Just $ case mode of
    Normal  -> formatMode myBlue        "#000000" "NORMAL "
    Resize  -> formatMode myGreen       "#000000" "RESIZE "
    Move    -> formatMode myMagenta     "#000000" "MOVE   "
    Focus   -> formatMode myOrange      "#000000" "FOCUS  "
    Prompt  -> formatMode myLightGreen  "#000000" "PROMPT "
    Launch  -> formatMode myTeal        "#000000" "LAUNCH "
    Media   -> formatMode myYellow      "#000000" "MEDIA  "
