{-# LANGUAGE ViewPatterns #-}

module QuarkInterpreter (runQuark, emptyQVM) where

import QuarkType
import QuarkParser
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import Data.Maybe
import Data.List
import System.Process
import Control.Exception


--- Evaluation ---

-- main evaluation function, takes a quark vm and (assuming there aren't any errors) returns its reduction
-- if the top item is a fuction, run it
-- otherwise, push the top item to the data stack
eval :: QVM -> IState
eval (stack, (viewl -> (QAtom a) :< sq), lib) = (getFunc lib a) (stack, sq, lib)
eval (stack, (viewl -> x :< sq), lib) =  return . Just $ (x : stack, sq, lib)

-- tries to retrieve a core or runtime defined function
-- if this fails, returns a `No such function: ` yeilding function
getFunc :: QLib -> String -> (QVM -> IState)
getFunc lib fname = case coreFunc fname of
  Just f -> f
  Nothing -> case libFunc fname lib of
    Just f -> f
    Nothing -> (\_ -> raiseError ("No such function: " ++ fname))

-- gets a runtime defined function, if no such function exists returns `Nothing`
libFunc :: String -> QLib -> Maybe (QVM -> IState)
libFunc var lib = Map.lookup var lib >>= (\f -> Just (\vm -> callQuote f vm))

-- this is the function responsible for the behavior of the `call` quark function
-- it is also used in `match` if a matching quote is found
callQuote :: QItem -> QVM -> IState
callQuote (QQuote args values v) vm = return . Just $ case patternMatch args (getStack vm) of
  Just bindings -> expandQuote values (Map.union v bindings) vm'
  Nothing -> pushVM (QSym "nil") vm'
  where vm' = dropVM (Seq.length args) vm
callQuote _ _ = raiseError "Tried to call a value that wasn't a quote"

expandQuote :: QProg -> QLib -> QVM -> QVM
expandQuote (viewr -> Seq.EmptyR) _ vm = vm
expandQuote (viewr -> sq :> (QAtom v)) vars (s, t, l) = expandQuote sq vars $ (s, v' <| t, l)
  where v' = case Map.lookup v vars of { Just x -> x; Nothing -> QAtom v; }
expandQuote (viewr -> sq :> (QQuote p b v)) vars (s, t, l) = expandQuote sq vars $ (s, q' <| t, l)
  where q' = QQuote p b (Map.union v vars)
expandQuote (viewr -> sq :> x) vars (s, t, l) = expandQuote sq vars $ (s, x <| t, l)

-- checks to make sure the items a quote is being applied to match the quotes pattern
-- if these items do match, it returns the variable bindings for the quote body
patternMatch :: Seq.Seq QItem -> QStack -> Maybe QLib
patternMatch pattern stack = qmatch Map.empty pattern stack
  where qmatch l (viewr -> Seq.EmptyR) _ = Just l
        qmatch l _ [] = Nothing
        qmatch l (viewr -> sq :> (QAtom x)) (y : ys) = if Map.member x l
          then if (l Map.! x) == y then qmatch l sq ys else Nothing
          else qmatch (Map.insert x y l) sq ys
        qmatch l (viewr -> sq :> (QQuote p b _)) ((QQuote p2 b2 _) : ys) = if (p == p2) && (b == b2) then qmatch l sq ys else Nothing
        qmatch l (viewr -> sq :> x) (y : ys) = if x == y then qmatch l sq ys else Nothing

-- concat items to a quark vm's token stack
fillQVM :: QVM -> QProg -> QVM
fillQVM (s, t, l) t' = (s, t' >< t, l)

-- a base quark vm, obviously all quark programs start with this
emptyQVM :: QVM
emptyQVM = ([], Seq.empty, Map.empty)


--- Core Function Utils ---


-- core function dispatch
coreFunc :: FuncName -> Maybe (QVM -> IState)
coreFunc = cf
  where
    -- numeric functions
    cf "+" = Just $ qNumFunc "+" (+)
    cf "*" = Just $ qNumFunc "*" (*)
    cf "/" = Just $ qNumFunc "/" (/)
    -- pure functions
    cf "<" = Just $ qPureFunc "<" [Num, Num] qlessthan
    cf "<<" = Just $ qPureFunc "<<" [Quote, Any] qpush
    cf ">>" = Just $ qPureFunc ">>" [Quote] qpop
    cf "@+" = Just $ qPureFunc "@+" [Quote, Quote] qunite
    cf "@-" = Just $ qPureFunc "@-" [Quote] qseparate
    cf "show" = Just $ qPureFunc "show" [Any] qshow
    cf "chars" = Just $ qPureFunc "chars" [Str] qchars
    cf "weld" = Just $ qPureFunc "weld" [Str, Str] qweld
    cf "type" = Just $ qPureFunc "type" [Any] qtypei
    cf "def" = Just $ qPureFunc "def" [Quote, Sym] qdef
    cf "parse" = Just $ qPureFunc "parse" [Str] qparsei
    -- impure functions
    cf "call" = Just $ qFunc "call" [Quote] qcall
    cf "match" = Just $ qFunc "match" [Quote] qmatch
    cf "." = Just $ qFunc "." [] qprintstack
    cf "load" = Just $ qFunc "load" [Str] qload
    cf "write" = Just $ qFunc "write" [Str, Str] qwrite
    cf "cmd" = Just $ qFunc "cmd" [Str] qcmd
    cf "print" = Just $ qFunc "print" [Str] qprint
    cf "exit" = Just $ qFunc "exit" [] qexit
    cf x = Nothing


-- Error Builders

-- raises an error in the quark interpreter
raiseError :: String -> IState
raiseError str = putStrLn ("ERROR: " ++ str) >> return Nothing

-- specific error function for type mis-matches of quark core functions
raiseTypeError :: String -> QTypeSig -> QStack -> IState
raiseTypeError name sig stack = raiseError ("Primary function: " ++ name ++ " expected a stack of: " ++ (show sig) ++ " but instead got: " ++ (show stack_sig))
  where stack_sig = map qtype . reverse . take (length sig) $ stack


-- Function Builders

-- wraps a quark function with type checking
qFunc :: FuncName -> QTypeSig -> (QVM -> IState) -> (QVM -> IState)
qFunc name sig f = (\vm -> if qtypeCheck (reverse sig) (getStack vm)
  then f vm
  else raiseTypeError name sig (getStack vm))

-- wraps a pure quark function with type checking
qPureFunc :: FuncName -> QTypeSig -> (QVM -> QVM) -> (QVM -> IState)
qPureFunc name sig f = qFunc name sig (return . Just . f)

-- wraps a numeric quark function with type checking
qNumFunc :: FuncName -> (Double -> Double -> Double) -> (QVM -> IState)
qNumFunc name f = qPureFunc name [Num, Num] f'
  where f' (QNum a : QNum b : s, t, l) = (QNum (f a b) : s, t, l)


--- Core Function Implementations ---


-- Pure Functions:

-- compares two numbers
qlessthan ((QNum x) : (QNum y) : s, t, l) = (QSym (if x < y then "true" else "false") : s, t, l)

-- pushes an item into a quote body
qpush (x : (QQuote a sq v) : s, t, l) = ((QQuote a (sq |> x) v) : s, t, l)

-- pops an item from a quote body
qpop ((QQuote a (viewr -> sq :> x) v) : s, t, l) = (x : (QQuote a sq v) : s, t, l)
qpop ((QQuote a (viewr -> EmptyR) v) : s, t, l) = ((QQuote a Seq.empty v) : s, t, l)

-- makes the body of the second quote the pattern of the first quote
qunite ((QQuote _ xs v) : (QQuote _ ys _) : s, t, l) = ((QQuote ys xs v) : s, t, l)

-- splits a quote into two new quotes, whose bodies contain the pattern and body of the original quote
qseparate ((QQuote ys xs v) : s, t, l) = ((QQuote Seq.empty xs v) : (QQuote Seq.empty ys Map.empty) : s, t, l)

-- pops an item, and pushes the type of this item as a symbol
qtypei (x : s, t, l) = (qtypeLiteral (qtype x) : s, t, l)

-- pops an item and pushes its string representation using serializeQ
qshow (x : s, t, l) = ((QStr (serializeQ x)) : s, t, l)

-- pops a string and pushes a quote containing a string for each character in the string
qchars ((QStr xs) : s, t, l) = (QQuote Seq.empty ((Seq.fromList . map (QStr . (\c -> [c]))) xs) Map.empty : s, t, l)

-- pops two strings and concats them
qweld ((QStr a) : (QStr b) : s, t, l) = ((QStr (b ++ a)) : s, t, l)

-- pops a symbol and quote. binds the symbol to the quote as a function in the vm
qdef ((QSym x) : y : s, t, l) = (s, t, Map.insert x y l)

-- evaluates a string of quark code by parsing it and concating these new values to the vm's token list
qparsei ((QStr x) : s, t, l) = case qParse x of
  Left _ -> ((QSym "not-ok") : s, t, l)
  Right qvals -> ((QSym "ok") : parsedQuote : s, t, l)
    where parsedQuote = QQuote Seq.empty (Seq.fromList qvals) Map.empty

-- Scary Impure Functions:

-- calls a quote
qcall (x : s, t, l) = callQuote x (s, t, l)

-- calls the first quote in a list of quotes that has a matching pattern
qmatch ((QQuote _ quotes _) : s, t, l) = callQuote (tryQuotes quotes s) (s, t, l)
  where tryQuotes (viewl -> Seq.EmptyL) _ = QQuote Seq.empty Seq.empty Map.empty
        tryQuotes (viewl -> (QQuote p q v) :< sq) s = case patternMatch p s of
          Just bindings -> (QQuote p q v)
          Nothing -> tryQuotes sq s

-- pops a string and prints it without a linebreak
qprint ((QStr x) : s, t, l) = putStr x >> return (Just (s, t, l))

-- prints the contents of the entire stack with a linebreak
qprintstack (s, t, l) = (putStrLn . intercalate " " . map serializeQ . reverse) s >> return (Just (s, t, l))

-- pops a string and loads the file with this filename, then pushes back the contents of the file as a string
qload ((QStr filename) : s, t, l) = do
  read_str <- try (readFile filename) :: IO (Either SomeException String)
  let stack_top = case read_str of {
    Left _ -> [QSym "not-ok"];
    Right s -> [QSym "ok", QStr s]; }
  return . Just $ (stack_top ++ s, t, l)

-- pops two strings, uses the first as a filename to save as and the second as the file contents
qwrite ((QStr filename) : (QStr toWrite) : s, t, l) = do
  wrote <- try (writeFile filename toWrite) :: IO (Either SomeException ())
  let stack_top = case wrote of {
    Left _ -> [QSym "not-ok"];
    Right _ -> [QSym "ok"]; }
  return . Just $ (stack_top ++ s, t, l)

-- pops a string and runs it as a shell command, pushes the output of the command as a string
qcmd ((QStr cmd) : s, t, l) = do
  result <- try (System.Process.readCreateProcess (System.Process.shell cmd) "") :: IO (Either SomeException String)
  let stack_top = case result of {
    Left _ -> [QSym "not-ok"];
    Right s -> [QSym "ok", QStr s]; }
  return . Just $ (stack_top ++ s, t, l)

-- exits the interpreter (only if in script mode)
qexit _ = return Nothing


--- Interpreter ---

-- Note: These functions would fit better in Quark.hs, but the `eval` core function needs `runQuark`, and mutually dependent modules are a pain...

-- parses and evaluates a string of quark code
runQuark :: Bool -> IO QVM -> String -> IO (Maybe QVM)
runQuark quiet iovm s = case qParse s of
  Left perror -> if not quiet then raiseError $ "Parse Error: " ++ (show perror) else return Nothing
  Right tokens -> do
    vm <- iovm
    let vm' = (return . Just) (fillQVM vm (Seq.fromList tokens)) in recEval vm'

-- reduces a quark vm until its token stack is empty
recEval :: IO (Maybe QVM) -> IO (Maybe QVM)
recEval iovm = do
  vm <- iovm
  case vm of
    Just (s, (viewl -> Seq.EmptyL), l) -> return $ Just (s, Seq.empty, l)
    Just x -> recEval $ eval x
    Nothing -> return Nothing
