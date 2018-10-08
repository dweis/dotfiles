import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

main = do
  xmproc <- spawnPipe "taffybar"
  xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        , terminal = "termite" }
        `additionalKeysP`
              [ ("M-p", spawn "x=$(dmenu -i -fn Hack) && exec $x")
              , ("M-q", spawn "xmonad --recompile && xmonad --restart")
              , ("M-f", spawn "firefox")
              , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 3%+")
              , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 3%-")
              , ("<XF86AudioMute>",        spawn "amixer sset Master toggle")
              , ("M-S-z", spawn "xscreensaver-command -lock; xset dpms force off")
              , ("M-S-p", spawn "sleep 0.2; scrot -s")
              , ("M-S-C-p", spawn "scrot")
              ]
