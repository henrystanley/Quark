module Quark.CoreFuncUtils where


import Quark.Type
import Quark.QType
import Quark.QVM
import Quark.QuoteEval
import Quark.Errors


--- Core Function Utilities ---


-- Function Builders

-- wraps a quark function with type checking
qFunc :: QTypeSig -> QFunc -> FuncName -> QFunc
qFunc sig f name = (\vm -> if qtypeCheck (reverse sig) (stack vm)
  then f vm
  else raiseTypeError name sig vm)

-- makes a pure quark function
qPureFunc :: QTypeSig -> (QVM -> QVM) -> FuncName -> QFunc
qPureFunc sig f name = qFunc sig (return . Just . f) name

-- makes a (QItem -> QItem) into a quark function
qOneToOne :: QTypeSig -> (QItem -> QItem) -> FuncName -> QFunc
qOneToOne sig f name = qPureFunc sig f' name
  where f' vm = let (x : xs) = stack vm in vm { stack = (f x) : xs }

-- makes a (QItem -> QItem -> QItem) into a quark function
qTwoToOne :: QTypeSig -> (QItem -> QItem -> QItem) -> FuncName -> QFunc
qTwoToOne sig f name = qPureFunc sig f' name
  where f' vm = let (x : y : xs) = stack vm in vm { stack = (f x y) : xs }

-- makes a (QItem -> [QItem]) into a quark function
qOneToMulti :: QTypeSig -> (QItem -> [QItem]) ->  FuncName -> QFunc
qOneToMulti sig f name = qPureFunc sig f' name
  where f' vm = let (x : xs) = stack vm in vm { stack = (f x) ++ xs }

-- makes a numeric quark function
qNumFunc :: (Double -> Double -> Double) -> FuncName -> QFunc
qNumFunc f name = qPureFunc [Num, Num] f' name
  where f' vm = let (QNum a : QNum b : xs) = stack vm in vm { stack = (QNum (f a b)) : xs }


-- Misc

-- operator for making function tuples in dispatch
(#=>) :: FuncName -> (FuncName -> QFunc) -> (FuncName, QFunc)
(#=>) name functionBuilder = (name, functionBuilder name)
