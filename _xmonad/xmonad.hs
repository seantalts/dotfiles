import XMonad
import XMonad.Config.Mate (mateConfig)
import XMonad.Prompt (defaultXPConfig, XPConfig, font, height)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol, ThreeColMid))
import XMonad.Hooks.DynamicLog (ppOutput, xmobarPP, dynamicLogWithPP)
import XMonad.Actions.WorkspaceNames (workspaceNamesPP)

xpConfig :: XPConfig
xpConfig =
  defaultXPConfig
  { font = "xft:Terminus:size=10:autohint=true"
  , height = 34
  }

customBindings :: [(String, X ())]
customBindings = [ ("M-p", shellPrompt xpConfig)
                 , ("M-S-p e", spawn "emacsclient -a '' -c")
                 , ("M-S-p f", spawn "firefox")
                 , ("M-S-p c", spawn "chromium")
		 , ("M-;", sendMessage Expand)
                 ]

main :: IO ()
main = do
  xmobar <- spawnPipe "xmobar"
  (xmonad . (`additionalKeysP` customBindings)) mateConfig
    { modMask = mod4Mask -- user Super instead of Alt
    , terminal = "mate-terminal --hide-menubar"
    , layoutHook = avoidStruts $ ThreeCol 1 (3/100) (1/2)
    ||| ThreeColMid 1 (3/100) (1/2)
    ||| Tall 1 (3/100) (1/2)
    ||| Full
    , manageHook = manageHook def <+> manageDocks
    , logHook = workspaceNamesPP xmobarPP {ppOutput = hPutStrLn xmobar} >>= dynamicLogWithPP
    }
