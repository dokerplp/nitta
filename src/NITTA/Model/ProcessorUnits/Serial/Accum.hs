{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Accum
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Serial.Accum
  ( accum
  , Accum
  , Ports(..), IOPorts(..)
  ) where

import           Control.Monad                   (when)
import           Data.Bits                       (finiteBitSize)
import           Data.Default
import           Data.List                       (find, partition, (\\))
import           Data.Set                        (elems, fromList, member)
import           Data.Maybe                      (fromMaybe)
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                (singleton, sup, (...))
import           Text.InterpolatedString.Perl6   (qc)

-- |Accumulator for each function
data Job v x = Job
        { tasks      :: [[( Bool,v )]]
        , current :: [[( Bool,v )]]
        , func       :: F v x
        }
    deriving (Eq, Show)

data Accum v x t = Accum
        { work                 :: [Job v x]
        , doneAt               :: Maybe t
        , currentWork          :: Maybe ( t, Job v x )
        , currentWorkEndpoints :: [ ProcessUid ]
        , process_             :: Process v x t
        , tick                 :: t
        , isInit               :: Bool
        }

instance (VarValTime v x t) => Show (Accum v x t) where
    show a = [qc|"
        Accum:
            work                 = {work a}
            doneAt               = {doneAt a}
            currentWork          = {currentWork a}
            currentWorkEndpoints = {currentWorkEndpoints a}
            process_             = {process_ a}
            tick_                = {tick a}
            isInit               = {isInit a}|]


accum :: (VarValTime v x t) => Accum v x t
accum = Accum
    { work=[]
    , doneAt=Nothing
    , currentWork=Nothing
    , currentWorkEndpoints=[]
    , process_=def
    , tick=def
    , isInit=True
    }


instance ( VarValTime v x t ) => Default (Accum v x t) where
    def = accum


setRemain f
    | Just (Acc vs) <- castF f = zip (pushStatusGroups vs) (pullStatusGroups vs)
    | otherwise                                = error "Error! Function is not Acc"


tryBindFunc f = Job {tasks = functionModel, current = [], func = f}
    where
        functionModel = concatMap (\(push, pull) -> [push, map (\x -> (False, x)) pull]) (setRemain f)


endpointOptionsFunc Job {tasks=[]} = []
endpointOptionsFunc Job {tasks=(t:_), current=[]} = map snd t
endpointOptionsFunc Job {tasks=(t:ts), current=(c:_)}
    | null ( t \\ c) && null ts = []
    | null $ t \\ c             = map snd $ head ts
    | otherwise                 = map snd $ t \\ c

endpointDecisionFunc a@Job {tasks=[]} _ = a
endpointDecisionFunc a@Job {tasks=tasks@(t:_), current=[]} v = a {tasks = updateTasks current' tasks, current=current'}
    where
        ([(neg, _)], _) = partition ((== v) . snd) t
        current' =[[(neg, v)]]

endpointDecisionFunc a@Job {tasks=tasks@(t:ts), current=(c:cs)} v
    | null $ t \\ c                      = endpointDecisionFunc a {tasks = ts} v
    | t \\ c /= t && length t > length c = a { tasks = updateTasks currentInsert tasks, current = currentInsert }
    | otherwise                          = a { tasks = updateTasks currentAdd tasks, current = currentAdd }
        where
            ([val], _) = partition ((== v) . snd) t
            currentInsert = (val : c) : cs
            currentAdd = [val] : c : cs

updateTasks (c:_) tasks@(t:ts)
    | null $ t \\ c = ts
    | otherwise     = tasks
updateTasks []    _ = error "Current is null"
updateTasks _     _ = error "Matching error updateTasks"


instance ( VarValTime v x t, Num x) => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu@Accum{work}
        | Just (Add a b c) <- castF f = Right pu{ work=tryBindFunc ( acc [Push Plus a, Push Plus b, Pull c] ) : work }
        | Just (Sub a b c) <- castF f = Right pu{ work=tryBindFunc ( acc [Push Plus a, Push Minus b, Pull c] ) : work }
        | Just Acc{}       <- castF f = Right pu{ work=tryBindFunc f : work}
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

    setTime t pu@Accum{} = pu{ tick=t }


instance ( VarValTime v x t, Num x) => EndpointProblem (Accum v x t) v t where
    endpointOptions Accum{ currentWork = Just (_, a@Job {tasks}), tick }
        | even (length tasks) = targets
        | odd (length tasks) = sources
            where
                targets = map (\v -> EndpointSt (Target v) $ TimeConstrain (tick+1 ... maxBound) (singleton 1)) (endpointOptionsFunc a)
                sources = [ EndpointSt (Source $ fromList (endpointOptionsFunc a) ) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]
    endpointOptions p@Accum{ work, currentWork = Nothing, tick } =
        concatMap (\a -> endpointOptions p {currentWork = Just (tick + 1, a)}) work

    endpointOptions _ = error "Error in matching in endpointOptions function"

    endpointDecision pu@Accum{ currentWork=Just (t, a@Job {tasks}), currentWorkEndpoints, isInit } d@EndpointSt{ epRole=Target v, epAt }
        | not (null tasks) && even ( length tasks )
        = let
                job@Job {tasks=newModel, current = (((neg, _):_):_)} = endpointDecisionFunc a v
                sel = if isInit then Init neg else Load neg
                (newEndpoints, process_') = runSchedule pu $ do
                    updateTick (sup epAt)
                    scheduleEndpoint d $ scheduleInstruction epAt sel
            in pu
                { process_=process_'
                , currentWork = Just(t, job)
                , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
                , doneAt=if null newModel
                    then Just $ sup epAt + 3
                    else Nothing
                , tick=sup epAt
                , isInit=null newModel
                }

    endpointDecision pu@Accum{ currentWork=Just (t, a@Job {tasks, current, func}), currentWorkEndpoints, doneAt} d@EndpointSt{ epRole=Source v, epAt }
        | not (null current) && odd ( length tasks ) = let
                job@Job {tasks=newModel} = foldl endpointDecisionFunc a (elems v)
                (newEndpoints, process_') = runSchedule pu $ do
                    endpoints <- scheduleEndpoint d $ scheduleInstruction (epAt-1) Out
                    when (null newModel) $ do
                        high <- scheduleFunction (t ... sup epAt) func
                        let low = endpoints ++ currentWorkEndpoints
                        establishVerticalRelations high low

                    updateTick (sup epAt)
                    return endpoints
            in pu
                { process_=process_'
                , doneAt=if null newModel then Nothing else doneAt
                , currentWork=if null newModel then Nothing else Just(t, job)
                , currentWorkEndpoints=if null newModel then [] else newEndpoints ++ currentWorkEndpoints
                , tick=sup epAt
                , isInit=null newModel
                }

    endpointDecision pu@Accum{work, currentWork=Nothing, tick} d
        | let v = oneOf $ variables d
        , Just job <- find (\Job {func} -> v `member` variables func) work
            = endpointDecision pu {work = work \\ [job], currentWork = Just (tick+1, job), isInit = True } d


    endpointDecision pu  d = error $ "error in Endpoint Decision function" ++ show pu ++ show d

instance Connected (Accum v x t) where
    data Ports (Accum v x t)
        = AccumPorts{ resetAcc, load, neg, oe :: SignalTag } deriving ( Show )


instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO deriving (Show)


instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)

    data Microcode (Accum v x t) =
        Microcode
            { oeSignal       :: Bool
            , resetAccSignal :: Bool
            , loadSignal     :: Bool
            , negSignal      :: Maybe Bool
            }
        deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} AccumPorts{..} =
        [ (resetAcc, Bool resetAccSignal)
        , (load, Bool loadSignal)
        , (oe, Bool oeSignal)
        , (neg, maybe Undef Bool negSignal)
        ]

    portsToSignals AccumPorts{ resetAcc, load, neg, oe } = [resetAcc, load, neg, oe]

    signalsToPorts (resetAcc:load:neg:oe:_) _ = AccumPorts resetAcc load neg oe
    signalsToPorts _                    _ = error "pattern match error in signalsToPorts AccumPorts"


instance Default (Microcode (Accum v x t)) where
    def = Microcode
        { oeSignal=False
        , resetAccSignal=False
        , loadSignal=False
        , negSignal=Nothing
        }


instance UnambiguouslyDecode (Accum v x t) where
    decodeInstruction (Init neg) = def{ resetAccSignal=True, loadSignal=True, negSignal=Just neg }
    decodeInstruction (Load neg) = def{ resetAccSignal=False, loadSignal=True, negSignal=Just neg }
    decodeInstruction Out        = def{ oeSignal=True }


instance (VarValTime v x t, Num x) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ f
    | Just f'@Add{} <- castF f = simulate cntx f'
    | Just f'@Sub{} <- castF f = simulate cntx f'
    | Just f'@Acc{} <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."


instance ( Var v ) => Locks (Accum v x t) v where
    locks Accum{ currentWork = Nothing }                  = []
    locks Accum{ currentWork = Just (_, job), work } = locks' job work
            where
                locks' Job{tasks=[]} _ = []
                locks' Job{current =[]} _ = []
                locks' Job{tasks=(m:ms), current =(r:_)} other =
                    [ Lock{ lockBy, locked }
                    | locked <- concatMap (map snd) ms ++ concatMap (concatMap (map snd) . tasks) other
                    , lockBy <- map snd (m \\ r)
                    ]


instance ( VarValTime v x t ) => TargetSystemComponent (Accum v x t) where
    moduleName _ _ = "pu_accum"
    hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
    software _ _ = Empty
    hardwareInstance tag _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst } AccumPorts{..} AccumIO
        = codeBlock [qc|
            pu_accum #
                    ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
                    , .ATTR_WIDTH( { show parameterAttrWidth } )
                    ) { tag }
                ( .clk( { signalClk } )
                , .rst( { signalRst } )
                , .signal_resetAcc( { signal resetAcc } )
                , .signal_load( { signal load } )
                , .signal_neg( { signal neg } )
                , .signal_oe( { signal oe } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io
        = error "Should be defined in network."

instance WithFunctions (Accum v x t) (F v x) where
    functions Accum{ process_, work}
        = functions process_
        ++ map func work


instance ( VarValTime v x t, Integral x
         ) => Testable (Accum v x t) v x where
    testBenchImplementation prj@Project{ pName, pUnit} = let
            tbcSignalsConst = ["resetAcc", "load", "oe", "neg"]

            showMicrocode Microcode{ resetAccSignal, loadSignal, oeSignal, negSignal } = codeBlock [qc|
                resetAcc   <= { bool2verilog resetAccSignal };
                load       <= { bool2verilog loadSignal };
                oe         <= { bool2verilog oeSignal };
                neg        <= { bool2verilog $ fromMaybe False negSignal };
                |]

            signal (SignalTag i) = case i of
                0 -> "resetAcc"
                1 -> "load"
                2 -> "oe"
                3 -> "neg"
                _ -> error "Can't match SignalTag in Accum testBenchImplementation"
            conf = SnippetTestBenchConf
                    { tbcSignals=tbcSignalsConst
                    , tbcPorts = AccumPorts
                        { resetAcc   = SignalTag 0
                        , load       = SignalTag 1
                        , oe         = SignalTag 2
                        , neg        = SignalTag 3
                        }
                    , tbcIOPorts=AccumIO
                    , tbcSignalConnect=signal
                    , tbcCtrl=showMicrocode
                    , tbDataBusWidth=finiteBitSize (def :: x)
                    }
        in Immediate (moduleName pName pUnit ++ "_tb.v") $ snippetTestBench prj conf

instance IOTestBench (Accum v x t) v x

instance RefactorProblem (Accum v x t) v x
