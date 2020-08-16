import XMonad
import XMonad.Config.Mate (mateConfig)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol, ThreeColMid))

customBindings :: [(String, X ())]
customBindings = [ ("M-p", shellPrompt defaultXPConfig)
                 , ("M-S-p e", spawn "emacsclient -a '' -c")
                 , ("M-S-p f", spawn "firefox")
                 , ("M-S-p c", spawn "chromium")
                 ]

main :: IO ()
main = do
  (xmonad . (`additionalKeysP` customBindings)) mateConfig
    { modMask = mod4Mask -- user Super instead of Alt
    , terminal = "mate-terminal --hide-menubar"
    , layoutHook = avoidStruts (ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/2))
    , manageHook = manageHook def <+> manageDocks
    , startupHook = spawn "xmobar"
    }
