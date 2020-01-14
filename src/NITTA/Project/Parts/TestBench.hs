{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Project.Parts.TestBench
Description : Generation a test bench for the target system.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.TestBench
    ( TestBench(..)
    , Testable(..), IOTestBench(..), TestbenchReport(..)
    , testBenchTopModuleName
    , projectFiles
    ) where

import qualified Data.HashMap.Strict              as HM
import qualified Data.List                        as L
import qualified Data.String.Utils                as S
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.Utils
import           NITTA.Project.Types
import           NITTA.Utils
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath.Posix            (joinPath)
import           Text.InterpolatedString.Perl6    (qc)


data TestBench = TestBench

instance ( Testable (m v x t) v x
        ) => ProjectPart TestBench (Project (m v x t) v x) where
    writePart TestBench prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        writeImplementation pPath $ testBenchImplementation prj


-- |Type class for all testable parts of a target system.
class Testable m v x | m -> v x where
    testBenchImplementation :: Project m v x -> Implementation


-- |Processor units with input/output ports should be tested by generation
-- external input ports signals and checking output port signals.
class IOTestBench pu v x | pu -> v x where
    testEnvironmentInitFlag :: String -> pu -> Maybe String
    testEnvironmentInitFlag _title _pu = Nothing

    testEnvironment :: String -> pu -> TargetEnvironment -> Ports pu -> IOPorts pu -> Cntx v x -> String
    testEnvironment _title _pu _env _ports _io _cntx = ""


data TestbenchReport v x
    = TestbenchReport
        { tbStatus                   :: Bool
        , tbPath                     :: String
        , tbFiles                    :: [ String ]
        , tbFunctions                :: [ String ]
        , tbSynthesisSteps           :: [ String ]
        , tbCompilerDump             :: [ String ]
        , tbSimulationDump           :: [ String ]
        , tbFunctionalSimulationCntx :: [ HM.HashMap v x ]
        , tbLogicalSimulationCntx    :: [ HM.HashMap v x ]
        }
    deriving ( Generic )


instance ( Show v, Show x ) => Show ( TestbenchReport v x ) where
    show TestbenchReport
            { tbPath, tbFiles
            , tbFunctions, tbSynthesisSteps
            , tbCompilerDump, tbSimulationDump
            }
        = codeBlock [qc|
            Project: { tbPath }
            Files:
                { inline $ showLst tbFiles }
            Functional blocks:
                { inline $ showLst tbFunctions }
            Steps:
                { inline $ showLst tbSynthesisSteps }
            compiler dump:
                { inline $ showLst tbCompilerDump }
            simulation dump:
                { inline $ showLst tbSimulationDump }
            |]
        where
            showLst = unlines . map ("    " ++)

-- |Generate list of project verilog files (including testbench).
projectFiles prj@Project{ pName, pUnit }
    = L.nub $ concatMap (addPath "") [ hardware pName pUnit, testBenchImplementation prj ]
    where
        addPath p (Aggregate (Just p') subInstances) = concatMap (addPath $ joinPath [p, p']) subInstances
        addPath p (Aggregate Nothing subInstances) = concatMap (addPath $ joinPath [p]) subInstances
        addPath p (Immediate fn _) = [ joinPath [ p, fn ] ]
        addPath _ (FromLibrary fn) = [ joinPath [ "lib", T.unpack $ L.last $ T.split (=='/') (T.pack fn) ] ]
        addPath _ Empty = []


-- |Get name of testbench top module.
testBenchTopModuleName prj = S.replace ".v" "" $ last $ projectFiles prj
