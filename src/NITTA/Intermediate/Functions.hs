{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Intermediate.Functions
Description : Functions for an application algorithm
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

В данном модуле описываются все функции доступные в системе. Функции (ранее
функциональные блоки) могут быть поддержаны вычислительными блоками в любых
вариантах (связь многие ко многим). Описание того, какие функции поддерживает
конретный PU можно посмотреть в:

- bindToState (класс SerialPUState) для последовательных вычислительных узлов;
- bind (класс ProcessorUnit) для остальных.

[@Функция (функциональный блок)@] Оператор прикладного алгоритма. Может обладать
внутренним состояние (между циклами), подразумевать внутренний процесс.
-}
module NITTA.Intermediate.Functions
    ( -- *Arithmetics
      Add(..), add
    , Division(..), division
    , Multiply(..), multiply
    , ShiftLR(..), shiftL, shiftR
    , Sub(..), sub
    -- *Memory
    , Constant(..), constant
    , Loop(..), loop, isLoop, LoopIn(..), LoopOut(..)
    , Reg(..), reg
    -- *Input/Output
    , Receive(..), receive
    , Send(..), send
    ) where

import qualified Data.Bits                as B
import           Data.Default
import           Data.List.Split          (splitWhen)
import           Data.Maybe               (mapMaybe)
import qualified Data.Map                 as M
import           Data.Set                 (elems, fromList, union)
import qualified Data.String.Utils        as S
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Utils



data Loop v x = Loop (X x) (O v) (I v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} ( Show x, Label v ) => Label (Loop v x) where
    label (Loop (X x) _ (I b)) = show x ++ "->" ++ label b
loop x a bs = F $ Loop (X x) (O $ fromList bs) $ I a
isLoop f
    | Just Loop{} <- castF f = True
    | otherwise = False

instance Function (Loop v x) v where
    isInternalLockPossible _ = True
    inputs  (Loop _ _a b) = variables b
    outputs (Loop _ a _b) = variables a
instance ( Ord v ) => Patch (Loop v x) (v, v) where
    patch diff (Loop x a b) = Loop x (patch diff a) (patch diff b)
instance ( Var v ) => Locks (Loop v x) v where
    locks (Loop _ (O as) (I b)) = [ Lock{ locked=b, lockBy=a } | a <- elems as ]
instance ( Var v ) => FunctionSimulation (Loop v x) v x where
    simulate cntx@CycleCntx{ cycleCntx } (Loop (X x) (O vs) (I _))
        = case cycleCntx M.!? oneOf vs of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _  -> return cntx
            -- if output variables are not defined - set initial value
            Nothing -> setZipX cntx vs x


data LoopOut v x = LoopOut (Loop v x) (O v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} ( Show v ) => Label (LoopOut v x) where
    label (LoopOut _ (O vs)) = show (oneOf vs) ++ " ->"
instance ( Ord v ) => Function (LoopOut v x) v where
    outputs (LoopOut _ o) = variables o
    isInternalLockPossible _ = True
instance ( Ord v ) => Patch (LoopOut v x) (v, v) where
    patch diff (LoopOut l a) = LoopOut (patch diff l) $ patch diff a
instance ( Var v ) => Locks (LoopOut v x) v where
    locks _ = []
instance ( Var v ) => FunctionSimulation (LoopOut v x) v x where
    simulate cntx (LoopOut l _) = simulate cntx l


data LoopIn v x = LoopIn (Loop v x) (I v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} ( Show v ) => Label (LoopIn v x) where
    label (LoopIn (Loop _ (O vs) _) (I v)) = "-> " ++ show v ++ " (" ++ show (oneOf vs) ++ ")"
instance ( Ord v ) => Function (LoopIn v x) v where
    inputs (LoopIn _ o) = variables o
    isInternalLockPossible _ = True
instance ( Ord v ) => Patch (LoopIn v x) (v, v) where
    patch diff (LoopIn l a) = LoopIn (patch diff l) $ patch diff a
instance ( Var v ) => Locks (LoopIn v x) v where locks (LoopIn l _) = locks l
instance ( Var v ) => FunctionSimulation (LoopIn v x) v x where
    simulate cntx (LoopIn l _) = simulate cntx l



data Reg v x = Reg (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Reg v x) where label Reg{} = "r"
instance ( Show v ) => Show (Reg v x) where
    show (Reg (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = reg(" ++ show k1 ++ ")"
reg a b = F $ Reg (I a) (O $ fromList b)

instance ( Ord v ) => Function (Reg v x) v where
    inputs  (Reg a _b) = variables a
    outputs (Reg _a b) = variables b
instance ( Ord v ) => Patch (Reg v x) (v, v) where
    patch diff (Reg a b) = Reg (patch diff a) (patch diff b)
instance ( Var v ) => Locks (Reg v x) v where
    locks = inputsLockOutputs
instance ( Var v ) => FunctionSimulation (Reg v x) v x where
    simulate cntx (Reg (I v) (O vs)) = do
        x <- cntx `getX` v
        setZipX cntx vs x


data Sign = Plus | Minus deriving (Show, Eq)

data Status s v = Push s (I v) | Pull (O v) deriving (Show, Eq)

newtype Acc s v x = Acc [Status s v] deriving (Eq)

instance {-# OVERLAPS #-} Label (Acc Sign v x) where label Acc{} = "+"
instance ( Show v) => Show (Acc Sign v x) where
    show (Acc lst) =  concatMap printStatus lst
        where
            printStatus :: (Show v) => Status Sign v -> String
            printStatus (Push Plus (I v))   = " +" ++ show v
            printStatus (Push Minus (I v))  = " -" ++ show v
            printStatus (Pull (O v))        = concatMap ((" => "++) . show) (elems v) ++ "; "

accum lst = F $ Acc lst

instance (Ord v) => Function (Acc Sign v x) v where
    inputs  (Acc lst) = fromList $ mapMaybe get lst
        where
            get s = case s of
                Push _ (I v) -> Just v
                _            -> Nothing
    outputs (Acc lst) = foldl1 union $ mapMaybe get lst
        where
            get s = case s of
                Pull (O v) -> Just v
                _          -> Nothing

instance ( Ord v ) => Patch (Acc Sign v x) (v, v) where
    patch diff (Acc lst) = Acc $ map
        (\case
            Push s v -> Push s (patch diff v)
            Pull vs  -> Pull (patch diff vs)
        ) lst

instance ( Var v ) => Locks (Acc Sign v x) v where
    locks = locksAcc

pushGroups (Acc lst) = map (map (\(Push _ (I v)) -> v)) $
    filter (/= []) $ splitWhen
        (\case
            Pull _ -> True
            _      -> False
        ) lst

pullGroups (Acc lst) = map (concatMap (elems . \(Pull (O v)) -> v)) $
    filter (/= []) $ splitWhen
        (\case
            Push _ _ -> True
            _        -> False
        ) lst

locksPush [] allLocks = filter ((/=[]) . fst) allLocks
locksPush (x:xs) [] = locksPush xs [([], x)]
locksPush (x:xs) allLocks@((lockedBefore, lockByBefore):_) = locksPush xs ((x, lockedBefore ++ lockByBefore ):allLocks)

locksPull [] allLocks = allLocks
locksPull ((lockedP, lockByP):xs) [] = locksPull xs [(lockedP, lockByP)]
locksPull ((lockedP, lockByP):xs) allLocks@((lockedBefore, lockByBefore):_) = locksPull xs ((lockedP, lockByP ++ lockedBefore ++ lockByBefore):allLocks)

locksAcc acc = let
        pushs = pushGroups acc
        pulls = pullGroups acc
        accTuple = zip pulls pushs
        locksListPush = locksPush pushs []
        locksListPull = locksPull accTuple []
        allLocks = locksListPush ++ locksListPull
    in
        concatMap (\eachLock ->
            [Lock { locked = y, lockBy = x}
            | x <- snd eachLock
            , y <- fst eachLock
            ]) allLocks


instance ( Var v, Num x ) => FunctionSimulation (Acc Sign v x) v x where
    simulate cntx (Acc lst) = let
            genPush v s acc context
                | Right x <- getX context v = case s of
                    Plus  -> (acc + x, Right context)
                    Minus -> (acc - x, Right context)
                | otherwise = (acc, Left "Error in accum Push value to context")

            select (acc, Right context) (Push s (I v)) = genPush v s acc context
            select (acc, Right context) (Pull (O vs)) = (acc, setZipX context vs acc)
            select (acc, Left err) _ = (acc, Left err)

            (_, eitherContext) = foldl select (0, Right cntx) lst
        in eitherContext

data Add v x = Add (I v) (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Add v x) where label Add{} = "+"
instance ( Show v ) => Show (Add v x) where
    show (Add (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " + " ++ show k2
add a b c = F $ Add (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Add v x) v where
    inputs  (Add  a  b _c) = variables a `union` variables b
    outputs (Add _a _b  c) = variables c
instance ( Ord v ) => Patch (Add v x) (v, v) where
    patch diff (Add a b c) = Add (patch diff a) (patch diff b) (patch diff c)
instance ( Var v ) => Locks (Add v x) v where
    locks = inputsLockOutputs
instance ( Var v, Num x ) => FunctionSimulation (Add v x) v x where
    simulate cntx (Add (I v1) (I v2) (O vs)) = do
        x1 <- cntx `getX` v1
        x2 <- cntx `getX` v2
        let x3 = x1 + x2 -- + 1 -- can be used for checking test working
        setZipX cntx vs x3



data Sub v x = Sub (I v) (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Sub v x) where label Sub{} = "-"
instance ( Show v ) => Show (Sub v x) where
    show (Sub (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " - " ++ show k2
sub a b c = F $ Sub (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Sub v x) v where
    inputs  (Sub  a  b _c) = variables a `union` variables b
    outputs (Sub _a _b  c) = variables c
instance ( Ord v ) => Patch (Sub v x) (v, v) where
    patch diff (Sub a b c) = Sub (patch diff a) (patch diff b) (patch diff c)
instance ( Var v ) => Locks (Sub v x) v where
    locks = inputsLockOutputs
instance ( Var v, Num x ) => FunctionSimulation (Sub v x) v x where
    simulate cntx (Sub (I v1) (I v2) (O vs)) = do
        x1 <- cntx `getX` v1
        x2 <- cntx `getX` v2
        let x3 = x1 - x2
        setZipX cntx vs x3



data Multiply v x = Multiply (I v) (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Multiply v x) where label Multiply{} = "*"
instance ( Show v ) => Show (Multiply v x) where
    show (Multiply (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " * " ++ show k2
multiply a b c = F $ Multiply (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Multiply v x) v where
    inputs  (Multiply  a  b _c) = variables a `union` variables b
    outputs (Multiply _a _b  c) = variables c
instance ( Ord v ) => Patch (Multiply v x) (v, v) where
    patch diff (Multiply a b c) = Multiply (patch diff a) (patch diff b) (patch diff c)
instance ( Var v ) => Locks (Multiply v x) v where
    locks = inputsLockOutputs
instance ( Var v, Num x ) => FunctionSimulation (Multiply v x) v x where
    simulate cntx (Multiply (I v1) (I v2) (O vs)) = do
        x1 <- cntx `getX` v1
        x2 <- cntx `getX` v2
        let x3 = x1 * x2
        setZipX cntx vs x3



data Division v x = Division
    { denom, numer     :: I v
    , quotient, remain :: O v
    } deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Division v x) where label Division{} = "/"
instance ( Show v ) => Show (Division v x) where
    show (Division (I k1) (I k2) (O k3) (O k4))
        =  S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " / " ++ show k2 ++ "; "
        ++ S.join " = " (if null k4 then ["_"] else map show $ elems k4) ++ " = " ++ show k1 ++ " `mod` " ++ show k2
division d n q r = F Division
    { denom=I d
    , numer=I n
    , quotient=O $ fromList q
    , remain=O $ fromList r
    }

instance ( Ord v ) => Function (Division v x) v where
    inputs  Division{ denom, numer } = variables denom `union` variables numer
    outputs Division{ quotient, remain } = variables quotient `union` variables remain
instance ( Ord v ) => Patch (Division v x) (v, v) where
    patch diff (Division a b c d) = Division (patch diff a) (patch diff b) (patch diff c) (patch diff d)
instance ( Var v ) => Locks (Division v x) v where
    locks = inputsLockOutputs
instance ( Var v, Integral x ) => FunctionSimulation (Division v x) v x where
    simulate cntx Division{ denom=I d, numer=I n, quotient=O qs, remain=O rs } = do
        dx <- cntx `getX` d
        nx <- cntx `getX` n
        let (qx, rx) = dx `quotRem` nx
        cntx' <- setZipX cntx qs qx
        setZipX cntx' rs rx



data Constant v x = Constant (X x) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} ( Show x ) => Label (Constant v x) where label (Constant (X x) _) = show x
instance ( Show v, Show x ) => Show (Constant v x) where
    show (Constant (X x) (O k)) = S.join " = " (map show $ elems k) ++ " = const(" ++ show x ++ ")"
constant x vs = F $ Constant (X x) $ O $ fromList vs

instance ( Show x, Eq x, Typeable x ) => Function (Constant v x) v where
    outputs (Constant _ o) = variables o
instance ( Ord v ) => Patch (Constant v x) (v, v) where
    patch diff (Constant x a) = Constant x (patch diff a)
instance ( Var v ) => Locks (Constant v x) v where locks _ = []
instance ( Var v ) => FunctionSimulation (Constant v x) v x where
    simulate cntx (Constant (X x) (O vs))
        = setZipX cntx vs x



-- FIXME: just fixme
data ShiftLR v x = ShiftL (I v) (O v)
                 | ShiftR (I v) (O v)
                deriving ( Typeable, Eq )
instance ( Show v ) => Show (ShiftLR v x) where
    show (ShiftL (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " << 1"
    show (ShiftR (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " >> 1"
shiftL a b = F $ ShiftL (I a) $ O $ fromList b
shiftR a b = F $ ShiftR (I a) $ O $ fromList b

instance ( Ord v ) => Function (ShiftLR v x) v where
    outputs (ShiftL i o) = variables i `union` variables o
    outputs (ShiftR i o) = variables i `union` variables o
instance ( Ord v ) => Patch (ShiftLR v x) (v, v) where
    patch diff (ShiftL a b) = ShiftL (patch diff a) (patch diff b)
    patch diff (ShiftR a b) = ShiftR (patch diff a) (patch diff b)
instance ( Var v ) => Locks (ShiftLR v x) v where
    locks = inputsLockOutputs
instance ( Var v, B.Bits x ) => FunctionSimulation (ShiftLR v x) v x where
    simulate cntx (ShiftL (I v1) (O vs)) = do
        x <- cntx `getX` v1
        let x' = x `B.shiftL` 1
        setZipX cntx vs x'
    simulate cntx (ShiftR (I v1) (O vs)) = do
        x <- cntx `getX` v1
        let x' = x `B.shiftR` 1
        setZipX cntx vs x'



newtype Send v x = Send (I v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} Label (Send v x) where label Send{} = "send"
send a = F $ Send $ I a
instance ( Ord v ) => Function (Send v x) v where
    inputs (Send i) = variables i
instance ( Ord v ) => Patch (Send v x) (v, v) where
    patch diff (Send a) = Send (patch diff a)
instance ( Var v ) => Locks (Send v x) v where locks _ = []
instance FunctionSimulation (Send v x) v x where
    simulate cntx Send{} = return cntx



newtype Receive v x = Receive (O v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} Label (Receive v x) where label Receive{} = "receive"
receive a = F $ Receive $ O $ fromList a
instance ( Ord v ) => Function (Receive v x) v where
    outputs (Receive o) = variables o
instance ( Ord v ) => Patch (Receive v x) (v, v) where
    patch diff (Receive a) = Receive (patch diff a)
instance ( Var v ) => Locks (Receive v x) v where locks _ = []
instance ( Var v, Val x ) => FunctionSimulation (Receive v x) v x where
    simulate cntx@CycleCntx{ cycleCntx } (Receive (O vs))
        = case cycleCntx M.!? oneOf vs of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _  -> return cntx
            -- if output variables are not defined - set initial value
            Nothing -> setZipX cntx vs def


-- *Internal

inputsLockOutputs f =
    [ Lock{ locked=y, lockBy=x }
    | x <- elems $ inputs f
    , y <- elems $ outputs f
    ]
