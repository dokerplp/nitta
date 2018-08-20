{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-
Есть следующие подходы к реализации множественных сетей:

1. Сеть представляется в виде вычислительного блока жадно вычисляющая все функции привязанные к ней.
Как следствие, она должна содержать в себе некоторый фрагмент компилятора. Наружу, в качестве опций
выдаются исключительные внешние взаимодействиясети. Соответсвенно любая привязка функционального
блока может сократить количество вариантов внутри сети, что требует особой обработки при принятие
решения компилятором. Обвязка для передачи данных реализуется автоматически и рассматривается как
встроенная часть интерфейса вычислительного блока с сетью. Все сети становятся вложенными друг
относительно друга.
2. Все коммуникационные сети представляются  как единое целое, разделённое на домены.
При биндинге решаются задачи модификации прикладного алгоритма для передачи данных между доменами
(если надо). Планирование вычислительного процесса производится в рамках отдельных доменов, а также
относительно пересылок данных между ними, при этом время в сетях должно быть максимально выравнено.
Любая сетевая структура становится плоской с точки зрения наблюдателя.
-}
module NITTA.BusNetwork where

import           Control.Monad.State
import qualified Data.Array                    as A
import           Data.Default
import           Data.List                     (find, nub, partition, sortOn,
                                                (\\))
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, isJust, mapMaybe)
import           Data.Set                      (elems, fromList, intersection)
import qualified Data.String.Utils             as S
import           Data.Typeable
import           NITTA.FunctionBlocks          (get', simulateAlgByCycle)
import           NITTA.Project
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval              (inf, width, (...))
import           Text.InterpolatedString.Perl6 (qq)

-- import           Debug.Trace


-- | Класс идентификатора вложенного вычислительного блока.
class ( Typeable v, Ord v, Show v ) => Title v
instance ( Typeable v, Ord v, Show v ) => Title v


data GBusNetwork title spu v x t
  = BusNetwork
    { -- | Список функциональных блоков привязанных к сети, но ещё не привязанных к конкретным
      -- вычислительным блокам.
      bnRemains        :: [FB (Parcel v x)]
    -- | Таблица привязок функциональных блоков ко вложенным вычислительным блокам.
    , bnBinded         :: M.Map title [FB (Parcel v x)]
    -- | Описание вычислительного процесса сети, как элемента процессора.
    , bnProcess        :: Process (Parcel v x) t
    -- | Словарь вложенных вычислительных блоков по именам.
    , bnPus            :: M.Map title spu
    -- | Ширина шины управления.
    , bnSignalBusWidth :: Int
    , bnInputPorts     :: [InputPort]
    , bnOutputPorts    :: [OutputPort]
    , bnAllowDrop      :: Maybe Bool
    }
type BusNetwork title v x t = GBusNetwork title (PU v x t) v x t

transfered net@BusNetwork{..}
  = [ v | st <- steps bnProcess
    , let instr = extractInstruction net st
    , isJust instr
    , let (Just (Transport v _ _)) = instr
    ]


-- TODO: Проверка подключения сигнальных линий.

-- TODO: Вариант функции, где провода будут подключаться автоматически.
busNetwork w allowDrop ips ops pus = BusNetwork [] (M.fromList []) def (M.fromList pus') w ips ops allowDrop
  where
    pus' = map (\(title, f) ->
      ( title
      , f Enviroment
        { signalClk="clk"
        , signalRst="rst"
        , signalCycle="cycle"
        , inputPort= \(InputPort n) -> n
        , outputPort= \(OutputPort n) -> n
        , net=NetEnv
          { parameterDataWidth=InlineParam "DATA_WIDTH"
          , parameterAttrWidth=InlineParam "ATTR_WIDTH"
          , dataIn="data_bus"
          , dataOut=title ++ "_data_out"
          , attrIn="attr_bus"
          , attrOut=title ++ "_attr_out"
          , signal= \(Signal i) -> "control_bus[" ++ show i ++ "]"
          }
        })
      ) pus

instance ( Title title
         , Time t
         , Var v
         , Typeable x
         ) => WithFunctionalBlocks (BusNetwork title v x t) (FB (Parcel v x)) where
  functionalBlocks BusNetwork{..} = sortFBs binded []
    where
      binded = bnRemains ++ concat (M.elems bnBinded)
      sortFBs [] _ = []
      sortFBs fbs cntx
        = let (ready, notReady) = partition (\fb -> insideOut fb || all (`elem` cntx) (inputs fb)) fbs
          in case ready of
            [] -> error "Cycle in algorithm!"
            _  -> ready ++ sortFBs notReady (elems (unionsMap outputs ready) ++ cntx)


instance ( Title title, Var v, Time t
         , Typeable x
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (BusNetwork title v x t)
         where
  options _proxy n@BusNetwork{..}
    = concat [ [ DataFlowO (srcTitle, fixPullConstrain pullAt) $ M.fromList pushs
               | pushs <- mapM pushOptionsFor $ elems pullVars
               , let pushTo = mapMaybe (fmap fst . snd) pushs
               , length (nub pushTo) == length pushTo
               ]
             | (srcTitle, opts) <- puOptions
             , EndpointO (Source pullVars) pullAt <- opts
             ]
    where
      now = nextTick bnProcess
      fixPullConstrain constrain
        = let a = max now $ constrain^.avail.infimum
              b = constrain^.avail.supremum
          in constrain & avail .~ (a ... b)

      pushOptionsFor v | v `notElem` availableVars = [(v, Nothing)]
      pushOptionsFor v = (v, Nothing) : pushOptionsFor' v

      pushOptionsFor' v = [ (v, Just (pushTo, pushAt))
                          | (pushTo, vars) <- puOptions
                          --  | (pushTo, vars) <- trace (S.join "\n" $ map show puOptions) puOptions
                          , EndpointO (Target pushVar) pushAt <- vars
                          , pushVar == v
                          ]
      bnForwardedVariables = transfered n
      availableVars =
        let fbs = bnRemains ++ concat (M.elems bnBinded)
            alg = foldl
                  (\dict (a, b) -> M.adjust ((:) b) a dict)
                  (M.fromList [(v, []) | v <- elems $ unionsMap variables fbs])
                  $ filter (\(_a, b) -> b `notElem` bnForwardedVariables)
                  $ concatMap dependency fbs
            notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
        in notBlockedVariables \\ bnForwardedVariables

      puOptions = M.assocs $ M.map (options endpointDT) bnPus

  decision _proxy n@BusNetwork{ bnProcess, bnPus } d@DataFlowD{ dfdSource=( srcTitle, pullAt ), dfdTargets }
    | nextTick bnProcess > d^.at.infimum
    = error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show (d^.at)
    | otherwise
    = let pushs = M.map (fromMaybe undefined) $ M.filter isJust dfdTargets
          transportStartAt = d^.at.infimum
          transportDuration = maximum $ map (\(_trg, time) -> (inf time - transportStartAt) + width time) $ M.elems pushs
          transportEndAt = transportStartAt + transportDuration

          subDecisions = ( srcTitle, EndpointD (Source $ fromList $ M.keys pushs) pullAt )
                       : [ ( trgTitle, EndpointD (Target v) pushAt )
                         | (v, (trgTitle, pushAt)) <- M.assocs pushs
                         ]
      in n{ bnPus=foldl applyDecision bnPus subDecisions
          , bnProcess=snd $ modifyProcess bnProcess $ do
              mapM_ (\(pushedValue, (targetTitle, _tc)) -> addStep
                      (Activity $ transportStartAt ... transportEndAt)
                      $ InstructionStep (Transport pushedValue srcTitle targetTitle :: Instruction (BusNetwork title v x t))
                    ) $ M.assocs pushs
              addStep_ (Activity $ transportStartAt ... transportEndAt) $ CADStep $ show d
              setProcessTime $ d^.at.supremum + 1
          }
    where
      applyDecision pus (trgTitle, d') = M.adjust (\pu -> decision endpointDT pu d') trgTitle pus



instance ( Title title, Time t, Var v, Typeable x
         ) => ProcessUnit (BusNetwork title v x t) (Parcel v x) t where

  tryBind fb bn@BusNetwork{..}
    | any (allowToProcess fb) $ M.elems bnPus
    = Right bn{ bnRemains=fb : bnRemains }
  tryBind fb BusNetwork{..} = Left $ "All sub process units reject the functional block: " ++ show fb ++ "\n"
                                ++ rejects
    where
      rejects = S.join "\n" $ map showReject $ M.assocs bnPus
      showReject (title, pu) | Left err <- tryBind fb pu = "    [" ++ show title ++ "]: " ++ err
      showReject (title, _) = "    [" ++ show title ++ "]: undefined"

  process pu@BusNetwork{..} = let
    transportKey = M.fromList
      [ (v, sKey st)
      | st <- steps bnProcess
      , let instr = extractInstruction pu st
      , isJust instr
      , let (Just (Transport v _ _)) = instr
      ]
    p'@Process{ steps=steps' } = snd $ modifyProcess bnProcess $ do
      let pus = sortOn fst $ M.assocs bnPus
      mapM (addSubProcess transportKey) pus

    in p'{ steps=reverse steps' }
    where
      addSubProcess transportKey (puTitle, pu') = do
        let subSteps = steps $ process pu'
        uids' <- foldM (\dict Step{..} -> do
                           k <- addStep sTime $ NestedStep puTitle sDesc
                           when (isFB sDesc) $ do
                             let FBStep fb = sDesc
                             mapM_ (\v -> when (v `M.member` transportKey)
                                          $ relation $ Vertical (transportKey M.! v) k
                                   ) $ variables fb
                           return $ M.insert sKey k dict
                       ) M.empty subSteps
        let subRelations = relations $ process pu'
        mapM (\(Vertical a b) -> relation $ Vertical (uids' M.! a) (uids' M.! b)
             ) subRelations

  setTime t bn@BusNetwork{..} = bn{ bnProcess=bnProcess{ nextTick=t }
                                  , bnPus=M.map (setTime t) bnPus
                                  }



instance Controllable (BusNetwork title v x t) where
  data Instruction (BusNetwork title v x t)
    = Transport v title title
    deriving (Typeable, Show)

  data Microcode (BusNetwork title v x t)
    = BusNetworkMC (A.Array Signal Value)


instance {-# OVERLAPS #-}
         ( Time t
         ) => ByTime (BusNetwork title v x t) t where
  microcodeAt BusNetwork{..} t
    = BusNetworkMC $ foldl merge initSt $ M.elems bnPus
    where
      initSt = A.listArray (Signal 0, Signal $ bnSignalBusWidth - 1) $ repeat def
      merge st PU{ unit, links }
        = foldl merge' st $ transmitToLink (microcodeAt unit t) links
      merge' st (s, x) = st A.// [ (s, st A.! s +++ x) ]



instance ( Title title, Var v, Time t ) => Simulatable (BusNetwork title v x t) v x where
  simulateOn cntx BusNetwork{..} fb
    = let Just (title, _) = find (\(_, v) -> fb `elem` v) $ M.assocs bnBinded
          pu = bnPus M.! title
      in simulateOn cntx pu fb



----------------------------------------------------------------------


-- | Функция позволяет проанализировать сеть и получить наружу варианты для управления привязками
-- функциональных блоков к вычислительным блокам. В текущем виде место для данной функции не
-- определено, так как:
--
-- 1. В случае если сеть выступает в качестве вычислительного блока, то она должна инкапсулировать
--    в себя эти настройки (но не hardcode-ить).
-- 2. Эти функции должны быть представленны классом типов.
instance ( Var v
         , Typeable x
         ) => DecisionProblem (BindingDT String (Parcel v x))
                    BindingDT (BusNetwork String v x t)
         where
  options _ BusNetwork{..} = concatMap bindVariants' bnRemains
    where
      bindVariants' fb =
        [ BindingO fb puTitle
        | (puTitle, pu) <- sortOn (length . binded . fst) $ M.assocs bnPus
        , allowToProcess fb pu
        , not $ selfTransport fb puTitle
        ]

      selfTransport fb puTitle =
        not $ null $ variables fb `intersection` unionsMap variables (binded puTitle)

      binded puTitle | puTitle `M.member` bnBinded = bnBinded M.! puTitle
                     | otherwise = []

  decision _ bn@BusNetwork{ bnProcess=p@Process{..}, ..} (BindingD fb puTitle)
    = bn{ bnPus=M.adjust (bind fb) puTitle bnPus
        , bnBinded=M.alter (\case Just fbs -> Just $ fb : fbs
                                  Nothing  -> Just [fb]
                           ) puTitle bnBinded
        , bnProcess=snd $ modifyProcess p $
            addStep (Event nextTick) $ CADStep $ "Bind " ++ show fb ++ " to " ++ puTitle
        , bnRemains=filter (/= fb) bnRemains
        }


--------------------------------------------------------------------------


instance ( Time t
         ) => TargetSystemComponent (BusNetwork String v x t) where
  moduleName title BusNetwork{..} = title ++ "_net"

  hardware title pu@BusNetwork{..}
    = let pus = map (uncurry hardware) $ M.assocs bnPus
          net = [ Immidiate (mn ++ ".v") iml
                , FromLibrary "pu_simple_control.v"
                ]
      in Aggregate (Just mn) (pus ++ net)
    where
      mn = moduleName title pu
      iml = let (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
            in [qq|{"module"} $mn
    #( parameter DATA_WIDTH = 32
     , parameter ATTR_WIDTH = 4
     )
    ( input                     clk
    , input                     rst
{ S.join "\\n" $ map (\\(InputPort p) -> ("    , input " ++ p)) bnInputPorts }
{ S.join "\\n" $ map (\\(OutputPort p) -> ("    , output " ++ p)) bnOutputPorts }
    , output              [7:0] debug_status
    , output              [7:0] debug_bus1
    , output              [7:0] debug_bus2
    , input                     is_drop_allow
    );

parameter MICROCODE_WIDTH = $bnSignalBusWidth;
// Sub module_ instances
wire [MICROCODE_WIDTH-1:0] control_bus;
wire [DATA_WIDTH-1:0] data_bus;
wire [ATTR_WIDTH-1:0] attr_bus;
wire cycle, start, stop;

wire [7:0] debug_pc;
assign debug_status = \{ cycle, debug_pc[6:0] \};
assign debug_bus1 = data_bus[7:0];
assign debug_bus2 = data_bus[31:24] | data_bus[23:16] | data_bus[15:8] | data_bus[7:0];

pu_simple_control
    #( .MICROCODE_WIDTH( MICROCODE_WIDTH )
     , .PROGRAM_DUMP( "\$path\${mn}.dump" )
     , .MEMORY_SIZE( {fromEnum (nextTick bnProcess) + 1} ) // 0 - address for nop microcode
     ) control_unit
    ( .clk( clk )
    , .rst( rst )
    , .start_cycle( { maybe "is_drop_allow" bool2binstr bnAllowDrop } || stop )
    , .cycle( cycle )
    , .signals_out( control_bus )
    , .trace_pc( debug_pc )
    );

{ S.join "\\n\\n" instances }

assign data_bus = { S.join " | " $ map snd valuesRegs };
assign attr_bus = { S.join " | " $ map fst valuesRegs };

endmodule
|]

      regInstance (t :: String)
        = [qq|wire [DATA_WIDTH-1:0] {t}_data_out;
wire [ATTR_WIDTH-1:0] {t}_attr_out;|]


      renderInstance insts regs [] = ( reverse insts, reverse regs )
      renderInstance insts regs ((t, PU{ unit, systemEnv, links }) : xs)
        = let inst = hardwareInstance t unit systemEnv links
              insts' = inst : regInstance t : insts
              regs' = (t ++ "_attr_out", t ++ "_data_out") : regs
          in renderInstance insts' regs' xs

  software title pu@BusNetwork{ bnProcess=Process{..}, ..}
    = let subSW = map (uncurry software) (M.assocs bnPus)
          sw = [ Immidiate (mn ++ ".dump") memoryDump ]
      in Aggregate (Just mn) $ subSW ++ sw
    where
      mn = moduleName title pu
      memoryDump = unlines $ map ( values2dump . values . microcodeAt pu ) ticks
      -- По нулевоу адресу устанавливается команда Nop (он же def) для всех вычислиетльных блоков.
      -- Именно этот адрес выставляется на сигнальные линии когда поднят сигнал rst.
      ticks = [ -1 .. nextTick - 1 ]
      values (BusNetworkMC arr) = reverse $ A.elems arr

  hardwareInstance = undefined


instance ( Title title, Var v, Time t
         , Show x
         , TargetSystemComponent (BusNetwork title v x t)
         , Typeable x
         ) => TestBench (BusNetwork title v x t) v x where
  testBenchDescription Project{ projectName, model=n@BusNetwork{..} } cntx0 = Immidiate (moduleName projectName n ++ "_tb.v") testBenchImp
    where
      ports = map (\(InputPort n') -> n') bnInputPorts ++ map (\(OutputPort n') -> n') bnOutputPorts
      testBenchImp = renderMST
        [ "`timescale 1 ps / 1 ps"
        , "module $moduleName$_tb();                                                                                 "
        , "                                                                                                          "
        , "reg clk, rst;                                                                                             "
        , if null ports
            then ""
            else "wire " ++ S.join ", " ports ++ ";"
        , ""
        , "$moduleName$                                                                                           "
        , "  #( .DATA_WIDTH( 32 )"
        , "   , .ATTR_WIDTH( 4 )"
        , "   ) net"
        , "  ( .clk( clk )                                                                                           "
        , "  , .rst( rst )                                                                                           "
        , S.join ", " ("  " : map (\p -> "." ++ p ++ "( " ++ p ++ " )") ports)
        , "// if 1 - The process cycle are indipendent from a SPI."
        , "// else - The process cycle are wait for the SPI."
        , "  , .is_drop_allow( " ++ maybe "is_drop_allow" bool2binstr bnAllowDrop ++ " )"
        , "  );                                                                                                      "
        , "                                                                                                          "
        , S.join "\n\n"
          [ tbEnv
          | (t, PU{ unit, systemEnv, links }) <- M.assocs bnPus
          , let t' = filter (/= '"') $ show t
          , let tbEnv = componentTestEnviroment t' unit systemEnv links
          , not $ null tbEnv
          ]
        , "                                                                                                          "
        , snippetDumpFile $ moduleName projectName n
        , "                                                                                                          "
        , snippetClkGen
        , "                                                                                                          "
        , "initial                                                                                                 "
        , "  begin                                                                                                 "
        , "    // microcode when rst == 1 -> program[0], and must be nop for all PUs                               "
        , "    @(negedge rst); // Turn nitta processor on.                                                         "
        , "    // Start computational cycle from program[1] to program[n] and repeat.                              "
        , "    // Signals effect to processor state after first clk posedge.                                       "
        , assertions
        , "  repeat ( 2000 ) @(posedge clk);"
        , "    \\$finish;                                                                                          "
        , "  end                                                                                                   "
        , "                                                                                                          "
        , "endmodule                                                                                                 "
        ]
        [ ( "moduleName", moduleName projectName n )
        ]

      -- TODO: Количество циклов для тестирования должно задаваться пользователем.
      cntxs = take 5 $ simulateAlgByCycle cntx0 $ functionalBlocks n
      cycleTicks = [ 0 .. nextTick (process n) - 1 ]
      simulationInfo = (0, def) : concatMap (\cntx -> map (, cntx) cycleTicks) cntxs
      assertions = concatMap ( ("    @(posedge clk); " ++) . (++ "\n") . assert ) simulationInfo
        where
          assert (t, cntx)
            = "\\$write(\"%s, bus actual: %h\", " ++ show (show t) ++ ", net.data_bus); "
            ++ case extractInstructionAt n t of
                Transport v _ _ : _
                  -> concat
                    [ "\\$write(\" expected: %h (%s)\", " ++ show (get' cntx v) ++ ", " ++ show v ++ ");"
                    , "if ( !( net.data_bus === " ++ show (get' cntx v) ++ ") ) "
                    ,   "\\$display(\" FAIL\"); else \\$display();"
                    ]
                [] -> "\\$display();"
