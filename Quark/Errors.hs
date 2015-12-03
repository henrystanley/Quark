module Quark.Errors where

import Quark.Type
import Quark.QVM
import Quark.QType

-- Error Builders

-- raises an error in the quark interpreter
raiseError :: String -> IState
raiseError str = putStrLn ("ERROR: " ++ str) >> return Nothing

-- specific error function for type mis-matches of quark core functions
raiseTypeError :: String -> QTypeSig -> QStack -> IState
raiseTypeError name sig stack = raiseError typeError
  where stack_sig = map qtype . reverse . take (length sig) $ stack
        typeError = "Primary function: "
                    ++ name
                    ++ " expected a stack of: "
                    ++ (show sig)
                    ++ " but instead got: "
                    ++ (show stack_sig)
