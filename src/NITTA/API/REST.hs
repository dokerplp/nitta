{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.API.REST
Description : REST API description for NITTA backend
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.API.REST
    ( Synthesis(..)
    , SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Zip      (mzip)
import           Data.Aeson
import           Data.Tree
import           GHC.Generics
import           NITTA.API.Marshalling  ()
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Project          (writeAndRunTestBench)
import           NITTA.Types            (F)
import           NITTA.Types.Project
import           NITTA.Types.Synthesis
import           Servant
import           System.FilePath        (joinPath)



-- *REST API Projections.

data SynthesisView
    = SynthesisView
        { svNnid     :: Nid
        , svCntx     :: [String]
        , svStatus   :: SynthesisStatus
        , svDuration :: Int
        }
    deriving (Generic)

instance ToJSON SynthesisView

view n = f <$> mzip (nidsTree n) n
    where
        f ( nid, Synthesis sModel sCntx sStatus _sCache ) = SynthesisView
            { svNnid=nid
            , svCntx=map show sCntx
            , svStatus=sStatus
            , svDuration=fromEnum $ targetProcessDuration sModel
            }



-- *REST API

type SynthesisAPI x t
    =    "synthesis" :> Get '[JSON] (Tree SynthesisView)
    :<|> "synthesis" :> Capture "nid" Nid :> WithSynthesis x t

synthesisServer st
    =    ( view <$> liftSTM (readTVar st))
    :<|> \nid -> withSynthesis st nid



type WithSynthesis x t
    =    Get '[JSON] (SynthesisTree String String x t)
    :<|> "model" :> Get '[JSON] (ModelState String String x t)
    :<|> "model" :> "alg" :> Get '[JSON] [F String x]
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] TestBenchReport
    :<|> SimpleCompilerAPI x t

withSynthesis st nid
    =    get st nid
    :<|> getModel st nid
    :<|> ( (\Frame{ dfg=DFG nodes } -> map (\(DFGNode f) -> f) nodes) <$> getModel st nid )
    :<|> ( \name -> getTestBenchOutput st nid name )
    :<|> simpleCompilerServer st nid



type SimpleCompilerAPI x t
    =    "simple" :> "options" :> Get '[JSON] [ WithMetric (CompilerDT String String x t) ]
    :<|> "simple" :> "obviousBind" :> Post '[JSON] Nid
    :<|> "simple" :> "allThreads" :> QueryParam' '[Required] "deep" Int :> Post '[JSON] Nid
    :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] Nid -- manualStep
    :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

simpleCompilerServer st nid
    =    simpleCompilerOptions st nid
    :<|> updateSynthesis (Just . synthesisObviousBind) st nid
    :<|> ( \deep -> updateSynthesis (Just . simpleSynthesisAllThreads simple deep) st nid )
    :<|> ( \ix -> updateSynthesis (apply (simpleSynthesisStep "manual") SynthesisStep{ setup=simple, ix=Just ix }) st nid )
    :<|> \case
            True -> updateSynthesis (apply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nid
            False -> updateSynthesis (Just . recApply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nid



get st nid = do
    root <- liftSTM $ readTVar st
    return $ getSynthesisNode nid root

simpleCompilerOptions st nid = do
    root <- liftSTM $ readTVar st
    let Synthesis{ sModel } = getSynthesis nid root
    return $ optionsWithMetrics simple sModel

getModel st nid = do
    root <- liftSTM $ readTVar st
    let Synthesis{ sModel } = getSynthesis nid root
    return sModel

updateSynthesis f st nid = liftSTM $ do
    n <- readTVar st
    case update f nid n of
        Just (n', nid') -> do
            writeTVar st n'
            return nid'
        Nothing -> return nid

getTestBenchOutput st _nid name = do
    Node{ rootLabel=Synthesis{ sModel } } <- liftSTM $ readTVar st
    let prj = Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor $ simpleSynthesis sModel
            , testCntx=Nothing
            , targetPlatforms=[ Makefile ]
            }
    liftIO $ writeAndRunTestBench prj


-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return e
