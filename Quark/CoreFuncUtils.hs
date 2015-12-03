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
  else raiseTypeError name sig (stack vm))

-- makes a pure quark function
qPureFunc :: QTypeSig -> (QVM -> QVM) -> FuncName -> QFunc
qPureFunc sig f name = qFunc sig (return . Just . f) name

-- makes a (QItem -> QItem) into a quark function
qOneToOne :: QTypeSig -> (QItem -> QItem) -> FuncName -> QFunc
qOneToOne sig f name = qPureFunc sig f' name
  where f' (QVM (x : xs) prog binds) = QVM ((f x) : xs) prog binds

-- makes a (QItem -> QItem -> QItem) into a quark function
qTwoToOne :: QTypeSig -> (QItem -> QItem -> QItem) -> FuncName -> QFunc
qTwoToOne sig f name = qPureFunc sig f' name
  where f' (QVM (x : y : xs) prog binds) = QVM (f x y : xs) prog binds

-- makes a (QItem -> [QItem]) into a quark function
qOneToMulti :: QTypeSig -> (QItem -> [QItem]) ->  FuncName -> QFunc
qOneToMulti sig f name = qPureFunc sig f' name
  where f' (QVM (x : xs) prog binds) = QVM ((f x) ++ xs) prog binds

-- makes a numeric quark function
qNumFunc :: (Double -> Double -> Double) -> FuncName -> QFunc
qNumFunc f name = qPureFunc [Num, Num] f' name
  where f' (QVM (QNum a : QNum b : stack) prog binds) = QVM (QNum (f a b) : stack) prog binds


-- Misc

-- operator for making function tuples in dispatch
(#=>) :: FuncName -> (FuncName -> QFunc) -> (FuncName, QFunc)
(#=>) name functionBuilder = (name, functionBuilder name)
