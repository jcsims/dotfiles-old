{-# LANGUAGE UnicodeSyntax #-}

import           Prelude.Unicode
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig     (additionalKeys)
import           XMonad.Util.Run          (spawnPipe)

myManagementHooks :: [ManageHook]
myManagementHooks = [resource =? "stalonetray" --> doIgnore]

main ∷ IO ()
main = do
  xmproc ← spawnPipe "xmobar"
  xmonad $ defaultConfig
    { borderWidth        = 2
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , manageHook = manageDocks <+> manageHook defaultConfig
                   <+> composeAll myManagementHooks
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" ∘  shorten 50}
    , startupHook = spawn "~/.xmonad/startup-hook"
    , modMask = mod4Mask -- Rebind mod key to the hyper key
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "i3lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    ]
