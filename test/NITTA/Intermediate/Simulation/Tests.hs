{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Intermediate.Simulation.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Simulation.Tests (
    tests,
) where

import Data.CallStack
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.List (permutations)
import Data.Maybe
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.TargetSystem ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

simulationTests =
    testGroup
        "functional simulation"
        [ testCase "reorder algorithm" $ do
            let f = reorderAlgorithm :: [F String Int] -> [F String Int]
                l1 = loop 0 "b2" ["a1"]
                l2 = loop 1 "c" ["b1", "b2"]
                a = add "a1" "b1" ["c"]
            mapM_ (([l1, l2, a] @=?) . f) $ permutations [l1, l2, a]
        , simulationTestCase
            "fibonacci sequence"
            def
            [ loop 0 "b2" ["a1"]
            , loop 1 "c" ["b1", "b2"]
            , add "a1" "b1" ["c"]
            ]
            ("a1", [0, 1, 1, 2, 3, 5, 8])
        , simulationTestCase
            "send and receive"
            [ ("a", [1, 2, 3, 4, 5])
            ]
            [ receive ["a"]
            , constant 10 ["b"]
            , add "a" "b" ["c"]
            , send "c"
            ]
            ("c", [11, 12, 13, 14, 15])
        ]

-- FIXME:
-- showTests =
--     testGroup
--         "show simulation result"
--         [ simulationTraceTestCase
--             "simple show"
--             [ loop 0 "b2" ["a1"]
--             , loop 1 "c" ["b1", "b2"]
--             , add "a1" "b1" ["c"]
--             ]
--             [__i|
--                 \n| Cycle  | a1  | b1  | b2  | c  |\n|:-------|:----|:----|:----|:---|\n| 1      | 0   | 1   | 1   | 1  |\n| 2      | 1   | 1   | 1   | 2  |\n| 3      | 1   | 2   | 2   | 3  |\n| 4      | 2   | 3   | 3   | 5  |\n| 5      | 3   | 5   | 5   | 8  |\n"
--                 |]
--         ]

tests =
    testGroup
        "intermediate simulation"
        [ simulationTests
        -- , showTests
        ]

simulationTestCase ::
    HasCallStack =>
    String ->
    [(String, [Int])] ->
    [F String Int] ->
    (String, [Int]) ->
    TestTree
simulationTestCase name received alg (v, expect) =
    testCase name $
        let dfg = fsToDataFlowGraph alg
            Cntx{cntxProcess} = simulateDataFlowGraph 5 def received dfg
            cycles = take (length expect) cntxProcess
            actual = map (\(CycleCntx c) -> fromMaybe (error $ show c) (HM.lookup v c)) cycles
         in expect @=? actual

simulationTraceTestCase ::
    HasCallStack =>
    String ->
    [F String Int] ->
    String ->
    TestTree
simulationTraceTestCase name alg expect =
    testCase name $
        let dfg = fsToDataFlowGraph alg
            cntx = simulateDataFlowGraph 5 def def dfg
            actual = show cntx
         in expect @=? actual
