import XMonad
import XMonad.Config.Mate (mateConfig)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)

customBindings :: [(String, X ())]
customBindings = [ ("M-p", shellPrompt defaultXPConfig)
                 , ("M-S-p e", spawn "emacsclient -a '' -c")
                 ]

main :: IO ()
main = do
  (xmonad . (`additionalKeysP` customBindings)) mateConfig
    { modMask = mod4Mask -- user Super instead of Alt
    , terminal = "mate-terminal --hide-menubar"
    , layoutHook = avoidStruts $ layoutHook def
    , manageHook = manageHook def <+> manageDocks
    , startupHook = spawn "xmobar"
    }
