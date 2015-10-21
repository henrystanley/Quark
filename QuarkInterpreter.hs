module QuarkInterpreter (runQuark, emptyQVM) where

import QuarkType
import QuarkParser
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import System.Process


--- Evaluation ---

-- main evaluation function, takes a quark vm and (assuming there aren't any errors) returns its reduction
eval :: QVM -> IO (Maybe QVM)
eval vm = case useTop vm of
  Just f -> f (dropTopToken vm) -- if the top item is a fuction, run it
  Nothing -> return . Just $ pushTop vm -- otherwise, push the top item to the data stack

-- used to check if the top item of the token list is a function, if it is, returns this function
useTop :: QVM -> Maybe (QVM -> IO (Maybe QVM))
useTop (stack, (QAtom a) : xs, lib) = case coreFunc a of
  Just f -> Just f
  Nothing -> let fromLib = libFunc a lib in case fromLib of
    Just f -> Just f
    Nothing -> Just (\_ -> raiseError ("No such function: " ++ a))
useTop vm = Nothing

-- drops the top item from the tokens list
dropTopToken :: QVM -> QVM
dropTopToken (s, [], l) = (s, [], l)
dropTopToken (s, t:ts, l) = (s, ts, l)

-- pushes the top item of the tokens stack to the data stack
pushTop :: QVM -> QVM
pushTop (stack, tokens, lib) = ((head tokens) : stack, tail tokens, lib)

-- map of strings to the core quark functions, returns `Nothing` if the string is not a core function
-- this seems like an inelegant way to do this, perhaps in the future I'll find a better way to implement it
coreFunc :: String -> Maybe (QVM -> IO (Maybe QVM))
coreFunc "+" = Just qadd
coreFunc "*" = Just qmult
coreFunc "/" = Just qdiv
coreFunc "<" = Just qlessthan
coreFunc "<<" = Just qpush
coreFunc ">>" = Just qpop
coreFunc "<@" = Just qargpush
coreFunc "@>" = Just qargpop
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
callQuote (QQuote args values) (stack, tokens, lib) = case patternMatch (reverse args) stack of
  Just bindings -> return . Just $ (drop (length args) stack, (libSub values bindings) ++ tokens, lib)
  Nothing -> return $ Just (QSym "nil" : (drop (length args) stack), tokens, lib)
callQuote x (s, t, l) = raiseError "Tried to call a value that wasn't a quote"

-- checks to make sure the items a quote is being applied to match the quotes pattern
-- if these items do match, it returns the variable bindings for the quote body
patternMatch :: [QItem] -> QStack -> Maybe QLib
patternMatch pattern stack = qmatch Map.empty pattern stack
  where qmatch l [] _ = Just l
        qmatch l _ [] = Nothing
        qmatch l (QAtom x : xs) (y : ys) = if Map.member x l
          then if (l Map.! x) == y then qmatch l xs ys else Nothing
          else qmatch (Map.insert x y l) xs ys
        qmatch l (x : xs) (y : ys) = if x == y then qmatch l xs ys else Nothing

-- before a quote body is evaluated, this function replaces variables with their binding from the quote pattern
libSub :: [QItem] -> QLib -> [QItem]
libSub [] _ = []
libSub (QAtom x : xs) l = if Map.member x l then (l Map.! x) : (libSub xs l) else (QAtom x) : (libSub xs l)
libSub (QQuote args items : xs) l = QQuote (libSub args l) (libSub items l) : (libSub xs l)
libSub (x : xs) l = x : (libSub xs l)

-- concat items to a quark vm's token stack
fillQVM :: QVM -> QStack -> QVM
fillQVM (s, t, l) t' = (s, t' ++ t, l)

-- a base quark vm, obviously all quark programs start with this
emptyQVM :: QVM
emptyQVM = ([], [], Map.empty)


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
qfunc name sig f = (\vm -> if qtypeCheck sig (getStack vm)
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
qpush = qfunc "<<" [Quote Any Any, Any] func
  where func (x : (QQuote a xs) : s, t, l) = return $ Just ((QQuote a (reverse (x : (reverse xs)))) : s, t, l)

-- pops an item from a quote body
qpop = qfunc ">>" [Quote Any NotEmpty] func
  where func ((QQuote a xs) : s, t, l) = return $ Just (( head . reverse $ xs) : (QQuote a (reverse . tail . reverse $ xs)) : s, t, l)

-- pushes an item into a quote pattern
qargpush = qfunc "<@" [Quote Any Any, Any] func
  where func (x : (QQuote a xs) : s, t, l) = return $ Just ((QQuote (reverse (x : (reverse a))) xs) : s, t, l)

-- pops an item from a quote pattern
qargpop = qfunc "@>" [Quote NotEmpty Any] func
  where func ((QQuote a xs) : s, t, l) = return $ Just (( head . reverse $ a) : (QQuote (reverse . tail . reverse $ a) xs) : s, t, l)

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
  where func ((QStr xs) : s, t, l) = return $ Just (QQuote [] (map (QStr . (\c -> [c])) xs) : s, t, l)

-- pops two strings and concats them
qweld = qfunc "weld" [Str, Str] func
  where func ((QStr a) : (QStr b) : s, t, l) = return $ Just ((QStr (b ++ a)) : s, t, l)

-- calls a quote
qcall = qfunc "call" [Quote Any Any] func
  where func (x : s, t, l) = (callQuote x (s, t, l))

-- calls the first quote in a list of quotes that has a matching pattern
qmatch = qfunc "match" [Quote Empty (Quote Any Any)] func
  where func ((QQuote _ quotes) : s, t, l) = callQuote (tryQuotes quotes s) (s, t, l)
        tryQuotes [] _ = QQuote [] []
        tryQuotes ((QQuote p q) : qs) s = case patternMatch (reverse p) s of
          Just bindings -> (QQuote p q)
          Nothing -> tryQuotes qs s


-- these are the scary non-pure functions:

-- pops a string and loads the file with this filename, then pushes back the contents of the file as a string
qload = qfunc "load" [Str] func
  where func ((QStr filename) : s, t, l) = do
          file <- readFile filename
          return $ Just (QStr file : s, t, l)

-- pops two strings, uses the first as a filename to save as and the second as the file contents
qwrite = qfunc "write" [Str, Str] func
  where func ((QStr filename) : (QStr toWrite) : s, t, l) = do
          writeFile filename toWrite
          return $ Just (s, t, l)

-- pops a string and runs it as a shell command, pushes the output of the command as a string
qcmd = qfunc "cmd" [Str] func
  where func ((QStr cmd) : s, t, l) = do
          result <- System.Process.readCreateProcess (System.Process.shell cmd) ""
          return $ Just (QStr result : s, t, l)

-- pops a symbol and quote. binds the symbol to the quote as a function in the vm
qdef = qfunc "def" [Quote Any Any, Sym] func
  where func ((QSym x) : y : s, t, l) = return $ Just (s, t, Map.insert x y l)

-- evaluates a string of quark code by parsing it and concating these new values to the vm's token list
qeval = qfunc "eval" [Str] func
  where func ((QStr x) : s, t, l) = do {
    evaled <- runQuark True (return (s, t, l)) x;
    case evaled of
      Nothing -> return $ Just ((QSym "not-ok") : s, t, l)
      Just (s', t', l') -> return $ Just ((QSym "ok") : s', t', l');
  }

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
    let vm' = (return . Just) (fillQVM vm tokens) in recEval vm'

-- reduces a quark vm until its token stack is empty
recEval :: IO (Maybe QVM) -> IO (Maybe QVM)
recEval iovm = do
  vm <- iovm
  case vm of
    Just (s, [], l) -> return $ Just (s, [], l)
    Just x -> recEval $ eval x
    Nothing -> return Nothing
