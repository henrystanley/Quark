module Quark.Errors where

import Quark.Type
import Quark.QVM
import Quark.QType

-- Error Builders

-- raises an error in the quark interpreter
raiseError :: String -> QVM -> IState
raiseError str vm = putStrLn ("ERROR: " ++ str ++ (formatCallstack vm)) >> return Nothing

formatCallstack :: QVM -> String
formatCallstack vm = "\nwith callstack:\n" ++ (unlines $ callstack vm)

-- specific error function for type mis-matches of quark core functions
raiseTypeError :: String -> QTypeSig -> QVM -> IState
raiseTypeError name sig vm = raiseError typeError vm
  where stack_sig = map qtype . reverse . take (length sig) $ stack vm
        typeError = "Primary function: "
                    ++ name
                    ++ " expected a stack of: "
                    ++ (show sig)
                    ++ " but instead got: "
                    ++ (show stack_sig)
