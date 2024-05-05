{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.Bind
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.Bind (
    BindMetrics (..),
    isSingleBind,
    isMultiBind,
    isObviousMultiBind,
) where

import Data.Aeson (ToJSON)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Endpoint
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Types
import NITTA.Utils
import Numeric.Interval.NonEmpty (inf)

data BindMetrics
    = SingleBindMetrics
        { pCritical :: Bool
        -- ^ Can this binding block another one (for example, one 'Loop' can
        --  take the last free buffer)?
        , pAlternative :: Float
        -- ^ How many alternative binding we have?
        , pRestless :: Float
        -- ^ How many ticks requires for executing the function?
        , pOutputNumber :: Float
        , pAllowDataFlow :: Float
        -- ^ How many transactions can be executed with this function?
        , pPossibleDeadlock :: Bool
        -- ^ May this binding cause deadlock?
        , pNumberOfBoundFunctions :: Float
        , pPercentOfBoundInputs :: Float
        -- ^ number of bound input variables / number of all input variables
        , pWave :: Maybe Float
        }
    | GroupBindMetrics
        { pOnlyObviousBinds :: Bool
        -- ^ We don't have alternatives for binding
        , pFunctionPercentInBinds :: Float
        -- ^ number of bound functions / number of all functions in DFG
        , pAvgBinds :: Float
        -- ^ average number of binds per unit
        , pVarianceBinds :: Float
        -- ^ variance of binds per unit
        , pAvgUnitWorkload :: Float
        -- ^ average number of variables after bind per unit
        , pVarianceUnitWorkload :: Float
        -- ^ variance of variables after bind per unit
        }
    deriving (Generic)

instance ToJSON BindMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetworks tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetworks tag v x t) tag v x t)
        (Bind tag v x)
        (Bind tag v x)
        BindMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, bindDecision sTarget o)]

    parameters
        SynthesisState
            { bindingAlternative
            , sTarget = sTarget@TargetSystem{mUnit}
            , possibleDeadlockBinds
            , bindWaves
            }
        (SingleBind tag f)
        _ =
            SingleBindMetrics
                { pCritical = isInternalLockPossible f
                , pAlternative = fromIntegral $ length (bindingAlternative M.! f)
                , pAllowDataFlow = fromIntegral $ length $ unionsMap variables $ filter isTarget $ optionsAfterBind f tag sTarget
                , pRestless = fromMaybe 0 $ do
                    (_var, tcFrom) <- L.find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables sTarget
                    return $ fromIntegral tcFrom
                , pOutputNumber = fromIntegral $ length $ S.elems $ outputs f
                , pPossibleDeadlock = f `S.member` possibleDeadlockBinds
                , pNumberOfBoundFunctions = fromIntegral $ length $ boundFunctions tag mUnit
                , pPercentOfBoundInputs =
                    let is = inputs f
                        n = fromIntegral $ length $ S.intersection is $ variables mUnit
                        nAll = fromIntegral $ length is
                     in if nAll == 0 then 1 else n / nAll
                , pWave = fmap fromIntegral $ case map (bindWaves M.!?) $ S.elems $ inputs f of
                    [] -> Just 0
                    waves | all isJust waves -> Just $ maximum $ catMaybes waves
                    _ -> Nothing
                }
    parameters SynthesisState{sTarget, unitWorkloadInFunction} binds@GroupBind{isObviousBinds, bindGroup} _ =
        let dfgFunCount = length $ functions $ mDataFlowGraph sTarget
            bindFunCount = length $ functions binds
         in GroupBindMetrics
                { pOnlyObviousBinds = isObviousBinds
                , pFunctionPercentInBinds = fromIntegral bindFunCount / fromIntegral dfgFunCount
                , pAvgBinds = avg $ map (fromIntegral . length . snd) $ M.assocs bindGroup
                , pVarianceBinds = stddev $ map (fromIntegral . length . snd) $ M.assocs bindGroup
                , pAvgUnitWorkload = avg $ map unitWorkload $ M.keys bindGroup
                , pVarianceUnitWorkload = stddev $ map unitWorkload $ M.keys bindGroup
                }
        where
            unitWorkload = fromIntegral . (unitWorkloadInFunction M.!)
            avg lst = sum lst / fromIntegral (length lst)
            stddev lst =
                let lstAvg = avg lst
                 in sqrt $ avg $ map (\x -> (x - lstAvg) ^ (2 :: Int)) lst

    estimate _ctx _o _d GroupBindMetrics{pOnlyObviousBinds, pFunctionPercentInBinds, pVarianceBinds} =
        sum
            [ 4100
            , pOnlyObviousBinds <?> 1000
            , fromInteger $ round pFunctionPercentInBinds * 10
            , fromInteger $ round pVarianceBinds * (-20)
            ]
    estimate _ctx _o _d SingleBindMetrics{pPossibleDeadlock = True} = 500
    estimate
        _ctx
        _o
        _d
        SingleBindMetrics
            { pCritical
            , pAlternative
            , pAllowDataFlow
            , pRestless
            , pNumberOfBoundFunctions
            , pWave
            , pPercentOfBoundInputs
            , pOutputNumber
            } =
            sum
                [ 3000
                , pCritical <?> 1000
                , (pAlternative == 1) <?> 500
                , pAllowDataFlow * 10
                , pPercentOfBoundInputs * 50
                , -fromMaybe (-1) pWave * 50
                , -pNumberOfBoundFunctions * 10
                , -pRestless * 4
                , pOutputNumber * 2
                ]

waitingTimeOfVariables net =
    [ (variable, inf $ tcAvailable constrain)
    | DataflowSt{dfSource = (_, srcEp), dfTargets} <- dataflowOptions net
    , let constrain = epAt srcEp
    , variable <- S.elems (variables srcEp S.\\ unionsMap (variables . snd) dfTargets)
    ]

optionsAfterBind f tag TargetSystem{mUnit = BusNetworks{bns}} =
    case tryBind f (bnPus (head bns) M.! tag) of
        Right pu' -> filter (\(EndpointSt act _) -> act `optionOf` f) $ endpointOptions pu'
        _ -> []
    where
        act `optionOf` f' = not $ S.null (variables act `S.intersection` variables f')

isSingleBind :: SynthesisDecision ctx m -> Bool
isSingleBind SynthesisDecision{metrics}
    | Just SingleBindMetrics{} <- cast metrics :: Maybe BindMetrics = True
isSingleBind _ = False

isMultiBind :: SynthesisDecision ctx m -> Bool
isMultiBind SynthesisDecision{metrics}
    | Just GroupBindMetrics{} <- cast metrics :: Maybe BindMetrics = True
isMultiBind _ = False

isObviousMultiBind :: SynthesisDecision ctx m -> Bool
isObviousMultiBind SynthesisDecision{metrics}
    | Just GroupBindMetrics{pOnlyObviousBinds = True} <- cast metrics :: Maybe BindMetrics = True
isObviousMultiBind _ = False
