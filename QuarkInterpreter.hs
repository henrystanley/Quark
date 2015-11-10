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
eval :: QVM -> IO (Maybe QVM)
eval (stack, (viewl -> (QAtom a) :< sq), lib) = (getFunc lib a) (stack, sq, lib)
eval (stack, (viewl -> x :< sq), lib) =  return . Just $ (x : stack, sq, lib)

-- tries to retrieve a core or runtime defined function
-- if this fails, returns a `No such function: ` yeilding function
getFunc :: QLib -> String -> (QVM -> IO (Maybe QVM))
getFunc lib fname = case coreFunc fname of
  Just f -> f
  Nothing -> case libFunc fname lib of
    Just f -> f
    Nothing -> (\_ -> raiseError ("No such function: " ++ fname))

-- map of strings to the core quark functions, returns `Nothing` if the string is not a core function
-- this is ugly, but I believe it is the fastest way to implement this kind of lookup
coreFunc :: String -> Maybe (QVM -> IO (Maybe QVM))
coreFunc "+" = Just qadd
coreFunc "*" = Just qmult
coreFunc "/" = Just qdiv
coreFunc "<" = Just qlessthan
coreFunc "<<" = Just qpush
coreFunc ">>" = Just qpop
coreFunc "@+" = Just qquotejoin
coreFunc "@-" = Just qquotesplit
coreFunc "." = Just qprintall
coreFunc "print" = Just qprint
coreFunc "show" = Just qshow
coreFunc "chars" = Just qchars
coreFunc "weld" = Just qweld
coreFunc "type" = Just qtypelang
coreFunc "load" = Just qload
coreFunc "write" = Just qwrite
coreFunc "cmd" = Just qcmd
coreFunc "call" = Just qcall
coreFunc "match" = Just qmatch
coreFunc "def" = Just qdef
coreFunc "eval" = Just qeval
coreFunc "exit" = Just qexit
coreFunc _ = Nothing

-- gets a runtime defined function, if no such function exists returns `Nothing`
libFunc :: String -> QLib -> Maybe (QVM -> IO (Maybe QVM))
libFunc var lib = Map.lookup var lib >>= (\f -> Just (\vm -> callQuote f vm))

-- this is the function responsible for the behavior of the `call` quark function
-- it is also used in `match` if a matching quote is found
callQuote :: QItem -> QVM -> IO (Maybe QVM)
callQuote (QQuote args values) (stack, tokens, lib) = case patternMatch args stack of
  Just bindings -> return . Just $ (drop (Seq.length args) stack, (libSub values bindings) >< tokens, lib)
  Nothing -> return $ Just (QSym "nil" : (drop (Seq.length args) stack), tokens, lib)
callQuote x (s, t, l) = raiseError "Tried to call a value that wasn't a quote"

-- checks to make sure the items a quote is being applied to match the quotes pattern
-- if these items do match, it returns the variable bindings for the quote body
patternMatch :: Seq.Seq QItem -> QStack -> Maybe QLib
patternMatch pattern stack = qmatch Map.empty pattern stack
  where qmatch l (viewr -> Seq.EmptyR) _ = Just l
        qmatch l _ [] = Nothing
        qmatch l (viewr -> sq :> (QAtom x)) (y : ys) = if Map.member x l
          then if (l Map.! x) == y then qmatch l sq ys else Nothing
          else qmatch (Map.insert x y l) sq ys
        qmatch l (viewr -> sq :> x) (y : ys) = if x == y then qmatch l sq ys else Nothing

-- before a quote body is evaluated, this function replaces variables with their binding from the quote pattern
libSub :: Seq.Seq QItem -> QLib -> Seq.Seq QItem
libSub (viewl -> Seq.EmptyL) _ = Seq.empty
libSub (viewl -> (QAtom x) :< sq) l = (if Map.member x l then (l Map.! x) else (QAtom x)) <| (libSub sq l)
libSub (viewl -> (QQuote args items) :< sq) l = QQuote (libSub args l) (libSub items l) <| (libSub sq l)
libSub (viewl -> x :< sq) l = x <| (libSub sq l)

-- concat items to a quark vm's token stack
fillQVM :: QVM -> QProg -> QVM
fillQVM (s, t, l) t' = (s, t' >< t, l)

-- a base quark vm, obviously all quark programs start with this
emptyQVM :: QVM
emptyQVM = ([], Seq.empty, Map.empty)


--- Core Function Utils ---

-- raises an error in the quark interpreter (who would have thought?)
raiseError :: String -> IO (Maybe QVM)
raiseError str = putStrLn ("ERROR: " ++ str) >> return Nothing

-- specific error function for type mis-matches of quark core functions
raiseTypeError :: String -> QTypeSig -> QStack -> IO (Maybe QVM)
raiseTypeError name sig stack = raiseError ("Primary function: " ++ name ++ " expected a stack of: " ++ (show sig) ++ " but instead got: " ++ (show stack_sig))
  where stack_sig = map qtype . reverse . take (length sig) $ stack

-- takes a quark function and typechecks it against the stack, if there are no errors it returns the function
qfunc :: String -> QTypeSig -> (QVM -> IO (Maybe QVM)) -> (QVM -> IO (Maybe QVM))
qfunc name sig f = (\vm -> if qtypeCheck (reverse sig) (getStack vm)
  then f vm
  else raiseTypeError name sig (getStack vm))

-- shortcut for defining quark core functions which are mathematical operators (Num -> Num -> Num)
qmathFunc :: String -> (Double -> Double -> Double) -> (QVM -> IO (Maybe QVM))
qmathFunc name f = qfunc name [Num, Num] (\((QNum y) : (QNum x) : s, t, l) -> return (Just (QNum (f x y) : s, t, l)))


--- Core Functions ---

-- the following are the nice pure-ish core functions:

-- addition
qadd = qmathFunc "+" (+)

-- subtraction
qmult = qmathFunc "*" (*)

-- multipication
qdiv = qmathFunc "/" (/)

-- compares two numbers
qlessthan = qfunc "<" [Num, Num] func
  where func ((QNum x) : (QNum y) : stack, t, l) = return $ Just (QSym (if x < y then "true" else "false") : stack, t, l)

-- pushes an item into a quote body
qpush = qfunc "<<" [Quote, Any] func
  where func (x : (QQuote a sq) : s, t, l) = return $ Just ((QQuote a (sq |> x)) : s, t, l)

-- pops an item from a quote body
qpop = qfunc ">>" [Quote] func
  where func ((QQuote a (viewr -> sq :> x)) : s, t, l) = return $ Just (x : (QQuote a sq) : s, t, l)
        func ((QQuote a (viewr -> EmptyR)) : s, t, l) = return $ Just ((QQuote a Seq.empty) : s, t, l)

-- pushes an item into a quote pattern
qquotejoin = qfunc "@+" [Quote, Quote] func
  where func ((QQuote _ xs) : (QQuote _ ys) : s, t, l) = return $ Just ((QQuote ys xs) : s, t, l)

-- pops an item from a quote pattern
qquotesplit = qfunc "@-" [Quote] func
  where func ((QQuote ys xs) : s, t, l) = return $ Just ((QQuote Seq.empty xs) : (QQuote Seq.empty ys) : s, t, l)

-- prints the contents of the entire stack with a linebreak
qprintall = qfunc "." [] func
  where func (s, t, l) = (putStrLn . intercalate " " . map serializeQ . reverse) s >> return (Just (s, t, l))

-- pops an item, and pushes the type of this item as a symbol, or in the case of quotes, as a quote
qtypelang = qfunc "type" [Any] func
  where func (x : s, t, l) = return $ Just (qtypeLiteral (qtype x) : s, t, l)

-- pops a string and prints it without a linebreak
qprint = qfunc "print" [Str] func
  where func ((QStr x) : s, t, l) = putStr x >> return (Just (s, t, l))

-- pops an item and pushes its string representation using serializeQ
qshow = qfunc "show" [Any] func
  where func (x : s, t, l) = return $ Just ((QStr (serializeQ x)) : s, t, l)

-- pops a string and pushes a quote containing a string for each character in the string
qchars = qfunc "chars" [Str] func
  where func ((QStr xs) : s, t, l) = return $ Just (QQuote Seq.empty ((Seq.fromList . map (QStr . (\c -> [c]))) xs) : s, t, l)

-- pops two strings and concats them
qweld = qfunc "weld" [Str, Str] func
  where func ((QStr a) : (QStr b) : s, t, l) = return $ Just ((QStr (b ++ a)) : s, t, l)

-- calls a quote
qcall = qfunc "call" [Quote] func
  where func (x : s, t, l) = (callQuote x (s, t, l))

-- calls the first quote in a list of quotes that has a matching pattern
qmatch = qfunc "match" [Quote] func
  where func ((QQuote _ quotes) : s, t, l) = callQuote (tryQuotes quotes s) (s, t, l)
        tryQuotes (viewl -> Seq.EmptyL) _ = QQuote Seq.empty Seq.empty
        tryQuotes (viewl -> (QQuote p q) :< sq) s = case patternMatch p s of
          Just bindings -> (QQuote p q)
          Nothing -> tryQuotes sq s


-- these are the scary non-pure functions:

-- pops a string and loads the file with this filename, then pushes back the contents of the file as a string
qload = qfunc "load" [Str] func
  where func ((QStr filename) : s, t, l) = do
          read_str <- try (readFile filename) :: IO (Either SomeException String)
          let stack_top = case read_str of {
            Left _ -> [QSym "not-ok"];
            Right s -> [QSym "ok", QStr s]; }
          return . Just $ (stack_top ++ s, t, l)

-- pops two strings, uses the first as a filename to save as and the second as the file contents
qwrite = qfunc "write" [Str, Str] func
  where func ((QStr filename) : (QStr toWrite) : s, t, l) = do
          wrote <- try (writeFile filename toWrite) :: IO (Either SomeException ())
          let stack_top = case wrote of {
            Left _ -> [QSym "not-ok"];
            Right _ -> [QSym "ok"]; }
          return . Just $ (stack_top ++ s, t, l)

-- pops a string and runs it as a shell command, pushes the output of the command as a string
qcmd = qfunc "cmd" [Str] func
  where func ((QStr cmd) : s, t, l) = do
          result <- try (System.Process.readCreateProcess (System.Process.shell cmd) "") :: IO (Either SomeException String)
          let stack_top = case result of {
            Left _ -> [QSym "not-ok"];
            Right s -> [QSym "ok", QStr s]; }
          return . Just $ (stack_top ++ s, t, l)

-- pops a symbol and quote. binds the symbol to the quote as a function in the vm
qdef = qfunc "def" [Quote, Sym] func
  where func ((QSym x) : y : s, t, l) = return $ Just (s, t, Map.insert x y l)

-- evaluates a string of quark code by parsing it and concating these new values to the vm's token list
qeval = qfunc "eval" [Str] func
  where func ((QStr x) : s, t, l) = do {
    evaled <- runQuark True (return (s, t, l)) x;
    case evaled of
      Nothing -> return $ Just ((QSym "not-ok") : s, t, l)
      Just (s', t', l') -> return $ Just ((QSym "ok") : s', t', l'); }

-- exits the interpreter (only if in script mode)
qexit = qfunc "exit" [] func
  where func _ = return Nothing


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
