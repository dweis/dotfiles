module Main where

import qualified Data.Map                            as M
import           Data.Ratio

import           System.Taffybar.Support.PagerHints  (pagerHints)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.EwmhDesktops           (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowNavigation

import           XMonad.Layout.BinarySpacePartition  as BSP

import qualified XMonad.StackSet as W

import           XMonad.Util.EZConfig                (additionalKeys,
                                                      additionalKeysP)

altMask    = mod1Mask
myModMask  = mod4Mask

--myTerminal = "termite"
myTerminal = "hyper"

-- Layouts
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout       = named "tall"     $ avoidStruts basicLayout
wideLayout       = named "wide"     $ avoidStruts $ Mirror basicLayout
singleLayout     = named "single"   $ avoidStruts $ noBorders Full
twoPaneLayout    = named "two pane" $ TwoPane (2/100) (1/2)
mosaicLayout     = named "mosaic"   $ MosaicAlt M.empty
gridLayout       = named "grid"     $ Grid

myLayoutHook = smartBorders $
               mkToggle (NOBORDERS ?? FULL ?? EOT)   $ tallLayout
                                                   ||| wideLayout
                                                   ||| singleLayout
                                                   ||| twoPaneLayout
                                                   ||| mosaicLayout
                                                   ||| gridLayout

myManageHook = composeAll [
    manageDocks
  --, fullscreenManageHook
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  , manageHook def
  ]

myStartupHook = do
  spawn "~/.xmonad/startup.sh"

main =
  xmonad $

       -- docks allows xmonad to handle taffybar
       docks $

       -- gives taffybar logger information
       ewmh $
       pagerHints

       -- xmonad config
       defaultConfig {
         modMask = myModMask
       , terminal = myTerminal
       , manageHook = myManageHook
       , layoutHook = myLayoutHook
       , startupHook = myStartupHook
       --, logHook = dbusLogWithPP client pp
       } `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
        , ("<XF86AudioMute>"       , spawn "amixer -q set Master toggle")
        , ("<XF86Launch1>"         , spawn "rofi -show combi")
        , ("<XF68ScreenSaver>"     , spawn "xscreensaver-command -lock; xset dpms force off")
        ]
   `additionalKeys`
        [ ((controlMask .|. altMask, xK_l) , spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print)         , spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print)                   , spawn "scrot")
        , ((myModMask, xK_p)               , spawn "rofi -show combi")
        , ((myModMask, xK_Left)            , prevWS)
        , ((myModMask, xK_Right), nextWS)
          -- Toggle current focus window to fullscreen
        , ((myModMask, xK_f), sendMessage $ Toggle FULL)
        , ((myModMask, xK_r), sendMessage BSP.Rotate)
        , ((myModMask, xK_s), sendMessage BSP.Swap) ]
