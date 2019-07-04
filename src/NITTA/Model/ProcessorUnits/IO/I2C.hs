{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.IO.I2C
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.IO.I2C
    ( Ports(..), I2CMode(..)
    , I2C, i2cUnit
    ) where

import           Control.Monad                             (when)
import           Data.Bits                                 (finiteBitSize)
import           Data.Default
import           Data.List                                 (find, partition,
                                                            (\\))
import qualified Data.Map                                  as M
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe)
import qualified Data.Set                                  as S
import qualified Data.String.Utils                         as S
import           NITTA.Intermediate.Functions
import qualified NITTA.Intermediate.Functions              as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits.Serial.Generic
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval                          (inf, sup, (...))
import           Text.InterpolatedString.Perl6             (qc)


data I2C v x t = I2C
        { bounceFilter  :: Int
        , bufferSize    :: Maybe Int -- ^if 'Nothing' then size should defined by algorithm
        , receiveQueue  :: [ Q v x ]
        , receiveN      :: Int
        , isReceiveOver :: Bool -- ^set if send buffer overlap receive buffer
        , sendQueue     :: [ Q v x ]
        , sendN         :: Int
        , process_      :: Process v x t
        }

data Q v x = Q{ vars :: [ v ], function :: F v x, cads :: [ ProcessUid ] }

i2cUnit :: ( Time t ) => Int -> I2C v x t
i2cUnit bounceFilter = I2C
    { bounceFilter
    , bufferSize=Just 6 -- FIXME:
    , receiveQueue=[]
    , receiveN=0
    , isReceiveOver=False
    , sendQueue=[]
    , sendN=0
    , process_=def
    }


instance ( VarValTime v x t
         ) => ProcessorUnit (I2C v x t) v x t where
    tryBind f i2c@I2C{ sendQueue, receiveQueue, receiveN, sendN, bufferSize }

        | Just F.Receive{} <- castF f, fromMaybe maxBound bufferSize == receiveN
        = Left $ "I2C to small buffer size"

        | Just F.Send{} <- castF f, fromMaybe maxBound bufferSize == sendN
        = Left $ "I2C to small buffer size"

        | Just (F.Receive (O vs)) <- castF f
        , let ( cads, process_ ) = runSchedule i2c $ scheduleFunctoinBind f
        = Right i2c
            { receiveQueue=Q{ vars=S.elems vs, function=f, cads } : receiveQueue
            , receiveN=receiveN + 1
            , process_
            }

        | Just (F.Send (I v)) <- castF f
        , let ( cads, process_ ) = runSchedule i2c $ scheduleFunctoinBind f
        = Right i2c
            { sendQueue=Q{ vars=[v], function=f, cads } : sendQueue
            , sendN=sendN + 1
            , process_
            }

        | otherwise = Left $ "I2C processor unit do not support: " ++ show f

    process = process_

    setTime t i2c@I2C{ process_ } = i2c{ process_=process_{ nextTick=t } }


instance ( VarValTime v x t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (I2C v x t)
        where

    options _proxy I2C{ receiveQueue, sendQueue, process_=Process{ nextTick } } = let
            source vs = EndpointO (Source $ S.fromList vs) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
            receiveOpts = map (source . vars) receiveQueue

            target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... 1)
            -- target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ...
            sendOpts = map (target . head . vars) sendQueue
        in receiveOpts ++ sendOpts


    decision _proxy i2c@I2C{ receiveQueue } d@EndpointD{ epdRole=Target v, epdAt }
        | ([ Q{ function } ], receiveQueue') <- partition (elem v . vars) receiveQueue
        , let ( _, process_ ) = runSchedule i2c $ do
                _ <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) Receiving
                updateTick (sup epdAt + 1)
                scheduleFunction (inf epdAt) (sup epdAt) function
        = i2c{ receiveQueue=receiveQueue', process_ }

    decision _proxy i2c@I2C{ sendQueue, sendN, receiveQueue, receiveN } d@EndpointD{ epdRole=Source vs, epdAt }
        | ([ Q{ function } ], sendQueue')
            <- partition ((\vs' -> vs' \\ S.elems vs /= vs') . vars) sendQueue
        , let ( _, process_ ) = runSchedule i2c $ do
                _ <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) Sending -- TODO: scheduleInstruction epdAt Send
                updateTick (sup epdAt + 1)
                scheduleFunction (inf epdAt) (sup epdAt) function -- TODO: scheduleFunction epdAt function
        = i2c
            { sendQueue=sendQueue'
            , isReceiveOver=(sendN - length sendQueue) >= (receiveN - length receiveQueue)
            , process_
            }

    decision _ _ _ = error "I2C model internal error"























instance Controllable (I2C v x t) where
    data Instruction (I2C v x t)
        = Receiving
        | Sending
        deriving ( Show )

    data Microcode (I2C v x t) = Microcode
            { wrSignal :: Bool
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} I2CPorts{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]


instance Default (Microcode (I2C v x t)) where
    def = Microcode
        { wrSignal=False
        , oeSignal=False
        }


instance UnambiguouslyDecode (I2C v x t) where
    decodeInstruction Sending   = def{ wrSignal=True }
    decodeInstruction Receiving = def{ oeSignal=True }



instance
        ( VarValTime v x t
        ) => Simulatable (I2C v x t) v x where
    simulateOn cntx _ f
        | Just f'@Receive{} <- castF f = simulate cntx f'
        | Just f'@Send{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on I2C."

data I2CMode
    = I2CMaster
    | I2CSlave
    deriving ( Show )

instance Connected (I2C v x t) where
    data Ports (I2C v x t)
        = I2CPorts
            { wr, oe :: SignalTag
             -- |Данный сигнал используется для оповещения процессора о завершении передачи данных. Необходимо для
             -- приостановки работы пока передача не будет завершена, так как в противном случае данные будут потеряны.
            , stop :: String
            , scl :: InputPortTag
            , sda :: InoutPortTag
            , mode :: I2CMode
            }
        deriving ( Show )

    -- externalInputPorts I2CPorts{ mode=I2CSlave, .. } = [ slave_mosi, slave_sclk, slave_cs ]
    -- externalInputPorts I2CPorts{ mode=I2CMaster, .. } = [ master_miso ]

    -- externalOutputPorts I2CPorts{ mode=I2CSlave, .. } = [ slave_miso ]
    -- externalOutputPorts I2CPorts{ mode=I2CMaster, .. } = [ master_mosi, master_sclk, master_cs ]


-- instance ( VarValTime v x t ) => TargetSystemComponent (SPI v x t) where
--     moduleName _ _ = "pu_spi"
--     hardware _tag _pu
--         = Aggregate Nothing
--             [ FromLibrary "spi/pu_slave_spi_driver.v"
--             , FromLibrary "spi/spi_slave_driver.v"
--             , FromLibrary "spi/spi_to_nitta_splitter.v"
--             , FromLibrary "spi/buffer.v"
--             , FromLibrary "spi/bounce_filter.v"
--             , FromLibrary "spi/spi_master_driver.v"
--             , FromLibrary "spi/nitta_to_spi_splitter.v"
--             , FromLibrary "spi/spi_to_nitta_splitter.v"
--             , FromLibrary "spi/pu_slave_spi.v"
--             , FromLibrary "spi/pu_master_spi.v"
--             ]
--     software _ pu = Immediate "transport.txt" $ show pu
--     hardwareInstance _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
--     hardwareInstance
--             tag
--             SerialPU{ spuState=State{ spiBounceFilter } }
--             TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort }
--             I2CPorts{ externalPorts, .. }
--         = fixIndent [qc|
-- |           { module_ externalPorts }
-- |               #( .DATA_WIDTH( { finiteBitSize (def :: x) } )
-- |                , .ATTR_WIDTH( { show parameterAttrWidth } )
-- |                , .BOUNCE_FILTER( { show spiBounceFilter } )
-- |                ) { tag }
-- |               ( .clk( { signalClk } )
-- |               , .rst( { signalRst } )
-- |               , .flag_stop( { stop } )
-- |               , .signal_cycle( { signalCycle } )
-- |               , .signal_oe( { signal oe } )
-- |               , .signal_wr( { signal wr } )
-- |               , .data_in( { dataIn } ), .attr_in( { attrIn } )
-- |               , .data_out( { dataOut } ), .attr_out( { attrOut } )
-- |               { extIO externalPorts }
-- |               );
-- |           |]
--             where
--                 module_ Slave{}  = "pu_slave_spi"
--                 module_ Master{} = "pu_master_spi"
--                 extIO Slave{..} = fixIndent [qc|
-- |                   , .mosi( { inputPort slave_mosi } )
-- |                   , .miso( { outputPort slave_miso } )
-- |                   , .sclk( { inputPort slave_sclk } )
-- |                   , .cs( { inputPort slave_cs } )
-- |           |]
--                 extIO Master{..} = fixIndent [qc|
-- |                   , .mosi( { outputPort master_mosi } )
-- |                   , .miso( { inputPort master_miso } )
-- |                   , .sclk( { outputPort master_sclk } )
-- |                   , .cs( { outputPort master_cs } )
-- |           |]


-- instance ( VarValTime v x t ) => IOTestBench (SPI v x t) v x where
--     testEnvironmentInitFlag tag _pu = Just $ tag ++ "_env_init_flag"

--     testEnvironment _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
--     testEnvironment
--             tag
--             pu@SerialPU{ spuState=State{ spiBounceFilter, spiReceive, spiSend } }
--             TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, inputPort, outputPort }
--             I2CPorts{ externalPorts, .. }
--             cntx@Cntx{ cntxCycleNumber, cntxProcess }
--         | let
--             receivedVariablesSeq = reverse $ map head $ fst spiReceive
--             receivedVarsValues = take cntxCycleNumber $ cntxReceivedBySlice cntx
--             sendedVariableSeq = reverse $ fst spiSend
--             sendedVarsValues = take cntxCycleNumber $ map cycleCntx cntxProcess

--             wordWidth = finiteBitSize (def :: x)
--             frameWordCount = max (length receivedVariablesSeq) (length $ sendedVariableSeq)
--             frameWidth = frameWordCount * wordWidth
--             timeLag = 10 :: Int

--             toVerilogLiteral xs = let
--                     xs' = map (\d -> [qc|{ wordWidth }'sd{ verilogInteger d }|]) xs
--                     placeholder = replicate (frameWordCount - length xs) [qc|{ wordWidth }'d00|]
--                 in S.join ", " (xs' ++ placeholder)

--             Just envInitFlagName = testEnvironmentInitFlag tag pu
--         = case externalPorts of
--             _ | frameWordCount == 0 -> ""
--             Slave{..} -> let
--                     receiveCycle transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
--                         in fixIndent [qc|
-- |
-- |                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
-- |                           { tag }_io_test_start_transaction = 1;                           @(posedge { signalClk });
-- |                           { tag }_io_test_start_transaction = 0;                           @(posedge { signalClk });
-- |                           repeat( { frameWidth * 2 + spiBounceFilter + 2 } ) @(posedge { signalClk });
-- |                   |]

--                     sendingAssert transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
--                         in fixIndent [qc|
-- |                           @(posedge { tag }_io_test_start_transaction);
-- |                               $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
-- |                               $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
-- |                               if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
-- |                                   $display("                       FAIL");
-- |                               $display();
-- |                       |]
--                 in fixIndent [qc|
-- |                   // SPI Input/Output environment
-- |                   // { show pu }
-- |                   reg { tag }_io_test_start_transaction;
-- |                   reg  [{ frameWidth }-1:0] { tag }_io_test_input;
-- |                   wire { tag }_io_test_ready;
-- |                   wire [{ frameWidth }-1:0] { tag }_io_test_output;
-- |                   initial { envInitFlagName } <= 0; // should be defined on the testbench level.
-- |                   spi_master_driver #
-- |                           ( .DATA_WIDTH( { frameWidth } )
-- |                           , .SCLK_HALFPERIOD( 1 )
-- |                           ) { tag }_io_test
-- |                       ( .clk( { signalClk } )
-- |                       , .rst( { signalRst } )
-- |                       , .start_transaction( { tag }_io_test_start_transaction )
-- |                       , .data_in( { tag }_io_test_input )
-- |                       , .data_out( { tag }_io_test_output )
-- |                       , .ready( { tag }_io_test_ready )
-- |                       , .mosi( { inputPort slave_mosi } )
-- |                       , .miso( { outputPort slave_miso } )
-- |                       , .sclk( { inputPort slave_sclk } )
-- |                       , .cs( { inputPort slave_cs } )
-- |                       );
-- |                   initial { tag }_io_test.inner.shiftreg <= 0;
-- |
-- |                   // SPI Input signal generation
-- |                   initial begin
-- |                       { tag }_io_test_start_transaction <= 0; { tag }_io_test_input <= 0;
-- |                       @(negedge { signalRst });
-- |                       repeat({ timeLag }) @(posedge { signalClk });
-- |                       { envInitFlagName } <= 1;
-- |                       { S.join "" $ map receiveCycle receivedVarsValues }
-- |                       repeat(70) @(posedge { signalClk });
-- |                       // $finish; // DON'T DO THAT (with this line test can pass without data checking)
-- |                   end
-- |
-- |                   // SPI Output signal checking
-- |                   initial begin
-- |                       @(negedge { signalRst });
-- |                       repeat (3) @(posedge { tag }_io_test_start_transaction); // latency
-- |                       { S.join "" $ map sendingAssert sendedVarsValues }
-- |                   end
-- |               |]
--             Master{..} -> let
--                     receiveCycle transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
--                         in fixIndent [qc|
-- |                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
-- |                           @(posedge { tag }_io_test_ready);
-- |                   |]

--                     sendingAssert transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
--                         in fixIndent [qc|
-- |
-- |                          @(posedge { tag }_io_test_ready);
-- |                                   $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
-- |                                   $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
-- |                                   if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
-- |                                       $display("                       FAIL");
-- |                                   $display();
-- |                       |]
--                 in fixIndent [qc|
-- |                   // SPI Input/Output environment
-- |                   // { show pu }
-- |                   reg { tag }_io_test_start_transaction;
-- |                   reg  [{ frameWidth }-1:0] { tag }_io_test_input;
-- |                   wire { tag }_io_test_ready;
-- |                   wire [{ frameWidth }-1:0] { tag }_io_test_output;
-- |                   initial { envInitFlagName } <= 0; // should be defined on the testbench level.
-- |                   spi_slave_driver #
-- |                           ( .DATA_WIDTH( { frameWidth } )
-- |                           ) { tag }_io_test_slave
-- |                       ( .clk( { signalClk } )
-- |                       , .rst( { signalRst } )
-- |                       , .data_in( { tag }_io_test_input )
-- |                       , .data_out( { tag }_io_test_output )
-- |                       , .ready( { tag }_io_test_ready )
-- |                       , .mosi( { outputPort master_mosi } )
-- |                       , .miso( { inputPort master_miso } )
-- |                       , .sclk( { outputPort master_sclk } )
-- |                       , .cs( { outputPort master_cs } )
-- |                       );
-- |
-- |                   // SPI Input signal generation
-- |                   initial begin
-- |                       @(negedge { signalRst });
-- |               { receiveCycle $ head receivedVarsValues }
-- |                       { envInitFlagName } <= 1;
-- |               { S.join "" $ map receiveCycle $ tail receivedVarsValues }
-- |                       repeat(70) @(posedge { signalClk });
-- |                       // $finish; // DON'T DO THAT (with this line test can pass without data checking)
-- |                   end
-- |
-- |                   // SPI Output signal checking
-- |                   initial begin
-- |                       @(negedge { signalRst });
-- |                       repeat(2) @(posedge { tag }_io_test_ready);
-- |                       { S.join "" $ map sendingAssert sendedVarsValues }
-- |                   end
-- |               |]
