module Quark.REPL (qRepl) where

import Quark.QVM
import Quark.Interpret
import Quark.Serialize
import qualified Data.Map as Map
import Data.List

--- REPL ---

-- read eval loop
-- prints prompt, gets input, then handles special cmds or evaulates the input
qRepl :: [String] -> IO QVM -> IO ()
qRepl params vm = do
  putStr ":> "
  input <- getLine
  case words input of
    ["*q"] -> return ()
    ["*f"] -> vm >>= displayFunctions >> qRepl params vm
    ["*f", f] -> vm >>= (displayFunction f) >> qRepl params vm
    ["*i", f] -> (if elem "-O" params
                    then vm >>= (displayInlineFunction f)
                    else putStrLn "Inlining disabled") >> qRepl params vm
    otherwise -> do
      reduced <- runQuark params vm input
      case reduced of
        Nothing -> qRepl params vm
        Just vm' -> do
          displayStack vm'
          qRepl params (return vm')


--- REPL Special Function Printing ---

-- prints the stack
displayStack :: QVM -> IO ()
displayStack vm = putStrLn stackStr
  where stackStr = intercalate " " . map (serializeQ 20) . reverse . stack $ vm

-- print all runtime defined functions from a QVM
displayFunctions :: QVM -> IO ()
displayFunctions vm = mapM putStr toPrint >> return ()
  where toPrint = map (\(f, v) -> f ++ "\n    " ++ (serializeQ 0 v) ++ "\n\n") $ Map.assocs (binds vm)

-- print a specific runtime defined function from a QVM
displayFunction :: String -> QVM -> IO ()
displayFunction fname vm = putStrLn $ case Map.lookup fname (binds vm) of
  Just func -> serializeQ 0 func
  Nothing -> "No such function: " ++ fname

-- print a specific runtime defined function in it's inlined form from a QVM
displayInlineFunction :: String -> QVM -> IO ()
displayInlineFunction fname vm = putStrLn $ case Map.lookup fname (i_binds vm) of
  Just (Just func) -> serializeQ 0 func
  Nothing -> "No such function: " ++ fname
