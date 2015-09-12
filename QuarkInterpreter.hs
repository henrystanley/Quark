module QuarkInterpreter (runQuark, emptyQVM) where

import QuarkType
import QuarkParser
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import System.Process


-- Evaluation --

eval :: QVM -> IO (Maybe QVM)
eval vm = case useTop vm of
  Just f -> f (dropTopToken vm)
  Nothing -> return . Just $ pushTop vm

useTop :: QVM -> Maybe (QVM -> IO (Maybe QVM))
useTop (stack, (QAtom a) : xs, lib) = case coreFunc a of
  Just f -> Just f
  Nothing -> libFunc a lib
useTop vm = Nothing

dropTopToken :: QVM -> QVM
dropTopToken (s, [], l) = (s, [], l)
dropTopToken (s, t:ts, l) = (s, ts, l)

pushTop :: QVM -> QVM
pushTop (stack, tokens, lib) = ((head tokens) : stack, tail tokens, lib)

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
coreFunc "raise" = Just qraise
coreFunc _ = Nothing


libFunc :: String -> QLib -> Maybe (QVM -> IO (Maybe QVM))
libFunc var lib = Map.lookup var lib >>= (\f -> Just (\vm -> callQuote f vm))

callQuote :: QItem -> QVM -> IO (Maybe QVM)
callQuote (QQuote args values) (stack, tokens, lib) = case patternMatch (reverse args) stack of
  Just bindings -> return . Just $ (drop (length args) stack, (libSub values bindings) ++ tokens, lib)
  Nothing -> return $ Just (QSym "nil" : stack, tokens, lib)
callQuote x (s, t, l) = raiseError "Tried to call a value that wasn't a quote"

patternMatch :: [QItem] -> QStack -> Maybe QLib
patternMatch pattern stack = qmatch Map.empty pattern stack
  where qmatch l [] _ = Just l
        qmatch l _ [] = Nothing
        qmatch l (QAtom x : xs) (y : ys) = if Map.member x l
          then if (l Map.! x) == y then qmatch l xs ys else Nothing
          else qmatch (Map.insert x y l) xs ys
        qmatch l (x : xs) (y : ys) = if x == y then qmatch l xs ys else Nothing

libSub :: [QItem] -> QLib -> [QItem]
libSub [] _ = []
libSub (QAtom x : xs) l = if Map.member x l then (l Map.! x) : (libSub xs l) else (QAtom x) : (libSub xs l)
libSub (QQuote args items : xs) l = QQuote (libSub args l) (libSub items l) : (libSub xs l)
libSub (x : xs) l = x : (libSub xs l)

fillQVM :: QVM -> QStack -> QVM
fillQVM (s, t, l) t' = (s, t' ++ t, l) 

emptyQVM :: QVM
emptyQVM = ([], [], Map.empty)


-- Core Function Utils --

raiseError :: String -> IO (Maybe QVM)
raiseError str = putStrLn ("ERROR: " ++ str) >> return Nothing

raiseTypeError :: String -> QTypeSig -> QStack -> IO (Maybe QVM)
raiseTypeError name sig stack = raiseError ("Primary function: " ++ name ++ " expected a stack of: " ++ (show sig) ++ " but instead got: " ++ (show stack_sig))
  where stack_sig = map qtype . reverse . take (length sig) $ stack

qfunc :: String -> QTypeSig -> (QVM -> IO (Maybe QVM)) -> (QVM -> IO (Maybe QVM))
qfunc name sig f = (\vm -> if qtypeCheck sig (getStack vm)
  then f vm
  else raiseTypeError name sig (getStack vm))

qmathFunc :: String -> (Double -> Double -> Double) -> (QVM -> IO (Maybe QVM))
qmathFunc name f = qfunc name [Num, Num] (\((QNum y) : (QNum x) : s, t, l) -> return (Just (QNum (f x y) : s, t, l)))


-- Core Functions --

qadd = qmathFunc "+" (+)

qmult = qmathFunc "*" (*)

qdiv = qmathFunc "*" (/)

qlessthan = qfunc "<" [Num, Num] func
  where func ((QNum x) : (QNum y) : stack, t, l) = return $ Just (QSym (if x < y then "true" else "false") : stack, t, l)

qpush = qfunc "<<" [Quote Any Any, Any] func
  where func (x : (QQuote a xs) : s, t, l) = return $ Just ((QQuote a (reverse (x : (reverse xs)))) : s, t, l)

qpop = qfunc ">>" [Quote Any NotEmpty] func
  where func ((QQuote a xs) : s, t, l) = return $ Just (( head . reverse $ xs) : (QQuote a (reverse . tail . reverse $ xs)) : s, t, l)
  
qargpush = qfunc "<@" [Quote Any Any, Any] func
  where func (x : (QQuote a xs) : s, t, l) = return $ Just ((QQuote (reverse (x : (reverse a))) xs) : s, t, l)

qargpop = qfunc "@>" [Quote NotEmpty Any] func
  where func ((QQuote a xs) : s, t, l) = return $ Just (( head . reverse $ a) : (QQuote (reverse . tail . reverse $ a) xs) : s, t, l)

qprintall = qfunc "." [] func
  where func (s, t, l) = (putStrLn . intercalate " " . map serializeQ . reverse) s >> return (Just (s, t, l))

qtypelang = qfunc "type" [Any] func
  where func (x : s, t, l) = return $ Just (qtypeLiteral (qtype x) : s, t, l)

qprint = qfunc "print" [Str] func
  where func ((QStr x) : s, t, l) = putStr x >> return (Just (s, t, l))
  
qshow = qfunc "show" [Any] func
  where func (x : s, t, l) = return $ Just ((QStr (serializeQ x)) : s, t, l)

qchars = qfunc "chars" [Str] func
  where func ((QStr xs) : s, t, l) = return $ Just (QQuote [] (map (QStr . (\c -> [c])) xs) : s, t, l)
  
qweld = qfunc "weld" [Str, Str] func
  where func ((QStr a) : (QStr b) : s, t, l) = return $ Just ((QStr (b ++ a)) : s, t, l)

qload = qfunc "load" [Str] func
  where func ((QStr filename) : s, t, l) = do
          file <- readFile filename
          return $ Just (QStr file : s, t, l)
          
qwrite = qfunc "write" [Str, Str] func
  where func ((QStr filename) : (QStr toWrite) : s, t, l) = do
          writeFile filename toWrite
          return $ Just (s, t, l)
          
qcmd = qfunc "cmd" [Str] func
  where func ((QStr cmd) : s, t, l) = do
          result <- System.Process.readCreateProcess (System.Process.shell cmd) ""
          return $ Just (QStr result : s, t, l)

qcall = qfunc "call" [Quote Any Any] func
  where func (x : s, t, l) = (callQuote x (s, t, l))

qmatch = qfunc "match" [Quote Empty (Quote Any Any)] func
  where func ((QQuote _ quotes) : s, t, l) = callQuote (tryQuotes quotes s) (s, t, l)
        tryQuotes [] _ = QQuote [] []
        tryQuotes ((QQuote p q) : qs) s = case patternMatch (reverse p) s of
          Just bindings -> (QQuote p q)
          Nothing -> tryQuotes qs s

qdef = qfunc "def" [Quote Any Any, Sym] func
  where func ((QSym x) : y : s, t, l) = return $ Just (s, t, Map.insert x y l)
  
qeval = qfunc "eval" [Str] func
  where func ((QStr x) : s, t, l) = do {
    evaled <- runQuark True (return (s, t, l)) x;
    case evaled of
      Nothing -> return $ Just ((QSym "not-ok") : s, t, l)
      Just (s', t', l') -> return $ Just ((QSym "ok") : s', t', l');
  }

qraise = qfunc "raise" [Str] func
  where func ((QStr x) : s, t, l) = raiseError x

-- Interpreter --

-- Note: These functions would fit better in Quark.hs, but the 'eval' core function needs 'runQuark', and mutually dependent modules are a pain...

runQuark :: Bool -> IO QVM -> String -> IO (Maybe QVM)
runQuark quiet iovm s = case qParse s of
  Left perror -> if not quiet then raiseError $ "Parse Error: " ++ (show perror) else return Nothing
  Right tokens -> do
    vm <- iovm
    let vm' = (return . Just) (fillQVM vm tokens) in recEval vm'
  
recEval :: IO (Maybe QVM) -> IO (Maybe QVM)
recEval iovm = do
  vm <- iovm
  case vm of
    Just (s, [], l) -> return $ Just (s, [], l)
    Just x -> recEval $ eval x
    Nothing -> return Nothing
