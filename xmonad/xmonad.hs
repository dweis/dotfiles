import XMonad
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks
import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

main = do
  tbproc <- spawnPipe "taffybar"

  xmonad $ 
    docks $
    ewmh $
    pagerHints defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn tbproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        , terminal = "termite"
        , borderWidth = 0
        }
        `additionalKeysP`
              [ ("M-p", spawn "x=$(dmenu -i -fn Hack) && exec $x")
              , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
              , ("M-S-f", spawn "firefox")
              , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 3%+")
              , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 3%-")
              , ("<XF86AudioMute>",        spawn "amixer sset Master toggle")
              , ("M-S-z", spawn "xscreensaver-command -lock; xset dpms force off")
              , ("M-S-p", spawn "sleep 0.2; scrot -s")
              , ("M-S-C-p", spawn "scrot")
              ]
