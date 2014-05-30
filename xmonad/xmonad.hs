{-# LANGUAGE UnicodeSyntax #-}

import           Data.Char                        (toLower)
import           Data.List                        (intercalate)
import           Prelude.Unicode
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.MouseResizableTile
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig             (additionalKeys)
import           XMonad.Util.NamedWindows         (getName)
import           XMonad.Util.Run                  (spawnPipe)

myManagementHooks :: [ManageHook]
myManagementHooks = [resource =? "stalonetray" --> doIgnore]

myLayout = avoidStruts $ mouseResizableTile ||| layoutHook defaultConfig

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        c <- withDisplay $ \d -> fmap resClass $ io $ getClassHint d w
        whenJust (W.findTag w ws) (flash name c)
      where flash _ "Pidgin" _ = spawn "true"
            flash _ "emesene" _ = spawn "true"
            flash name c index = spawn $
                                 intercalate " " $
                                 [ "notify-send -i"
                                 , icon
                                 , show $ show name
                                 , show $ "on " ++ index ]
                where icon = case c of
                               "URxvt" -> "gnome-terminal"
                               otherwise -> map toLower c

main ∷ IO ()
main = do
  xmproc ← spawnPipe "xmobar"
  xmonad $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
    { borderWidth        = 0
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , manageHook = manageDocks <+> manageHook defaultConfig
                   <+> composeAll myManagementHooks
    , layoutHook = myLayout
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
