{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.Frontend
Description : Lua frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Frontend
    ( lua2functions
    ) where

import           Control.Monad                 (when)
import           Control.Monad.State
import           Data.Default                  (def)
import           Data.List                     (find, group, sort)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes)
import qualified Data.String.Utils             as S
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text                     as T
import           Language.Lua
import qualified NITTA.Functions               as F
import           NITTA.Types                   (F, Parcel)
import           Text.InterpolatedString.Perl6 (qq)



lua2functions src
    = let
        ast = either (\e -> error $ "can't parse lua src: " ++ show e) id $ parseText chunk src
        Right (fn, call, funAssign) = findMain ast
        AlgBuilder{ algItems } = buildAlg $ do
            addMainInputs call funAssign
            let statements = funAssignStatments funAssign
            forM_ statements $ processStatement fn
            addConstants
        fs = filter (\case Function{} -> True; _ -> False) algItems
        varDict = M.fromList
            $ map varRow
            $ group $ sort $ concatMap fIn fs
    in snd $ execState (mapM_ (store <=< function2nitta) fs) (varDict, [])
    where
        varRow lst@(x:_)
            = let vs = zipWith (\v i -> [qq|{unpack v}_{i}|]) lst ([0..] :: [Int])
            in (x, (vs, vs))
        varRow _ = undefined



data AlgBuilder
    = AlgBuilder
        { algItems  :: [AlgBuilderItem]
        , algBuffer :: [AlgBuilderItem]
        , algVarGen :: [Text]
        , algVars   :: [Text]
        }

instance Show AlgBuilder where
    show (AlgBuilder algItems algBuffer _algVarGen algVars )
        = "AlgBuilder\n{ algItems=\n"
        ++ S.join "\n" (map show $ reverse algItems)
        ++ "\nalgBuffer: " ++ show algBuffer
        ++ "\nalgVars: " ++ show algVars
        ++ "\n}"

data AlgBuilderItem
    = InputVar{ iIx :: Int, iX :: Int, iVar :: Text }
    | Constant{ cX :: Int, cVar :: Text }
    | Alias{ aFrom :: Text, aTo :: Text }
    | Renamed{ rFrom :: Text, rTo :: Text }
    | Function
        { fIn     :: [Text]
        , fOut    :: [Text]
        , fName   :: String
        , fValues :: [Int]
        }
    deriving ( Show )



buildAlg proc
    = execState proc AlgBuilder
        { algItems=[]
        , algBuffer=[]
        , algVarGen=map (\i -> [qq|#$i|]) [(0::Int)..]
        , algVars=[]
        }



-- *Translate AlgBuiler functions to nitta functions

function2nitta Function{ fName="loop",     fIn=[i],    fOut=[o], fValues=[x] } = F.loop x <$> input i <*> output o
function2nitta Function{ fName="reg",      fIn=[i],    fOut=[o], fValues=[]  } = F.reg <$> input i <*> output o
function2nitta Function{ fName="constant", fIn=[],     fOut=[o], fValues=[x] } = F.constant x <$> output o
function2nitta Function{ fName="send",     fIn=[i],    fOut=[],  fValues=[]  } = F.send <$> input i
function2nitta Function{ fName="add",      fIn=[a, b], fOut=[c], fValues=[]  } = F.add <$> input a <*> input b <*> output c
function2nitta Function{ fName="sub",      fIn=[a, b], fOut=[c], fValues=[]  } = F.sub <$> input a <*> input b <*> output c
function2nitta Function{ fName="multiply", fIn=[a, b], fOut=[c], fValues=[]  } = F.multiply <$> input a <*> input b <*> output c
function2nitta Function{ fName="divide",   fIn=[d, n], fOut=[q, r], fValues=[] } = F.division <$> input d <*> input n <*> output q <*> output r
function2nitta f = error $ "unknown function: " ++ show f



input v = do
    (dict, fs) <- get
    let (x:xs, lst) = dict M.! v
    put (M.insert v (xs, lst) dict, fs)
    return x

output v
    | T.head v == '_' = return []
    | otherwise = gets $ \(dict, _fs) -> snd (dict M.! v)

store f = modify' $ \(dict, fs) -> (dict, f:fs)



-- *AST inspection and algorithm builder

findMain (Block statements Nothing)
    | [call] <- filter (\case FunCall{} -> True; _ -> False) statements
    , [funAssign] <- filter (\case FunAssign{} -> True; _ -> False) statements
    , (FunCall (NormalFunCall (PEVar (VarName (Name fnCall))) _)) <- call
    , (FunAssign (FunName (Name fnAssign) _ _) _) <- funAssign
    , fnCall == fnAssign
    = Right (fnCall, call, funAssign)
findMain _ = error "can't find main function in lua source code"


addMainInputs
        (FunCall (NormalFunCall _ (Args callArgs)))
        (FunAssign (FunName (Name _funName) _ _) (FunBody declArgs _ _)) = do
    let vars = map (\case (Name v) -> v) declArgs
    let values = map (\case (Number _ s) -> read (unpack s); _ -> undefined) callArgs
    when (length vars /= length values)
        $ error "a different number of arguments in main a function declaration and call"
    mapM_ (\(iIx, iX, iVar) -> addItem InputVar{ iIx, iX, iVar } [iVar]) $ zip3 [0..] values vars

addMainInputs _ _ = error "bad main function description"



addConstants = do
    AlgBuilder{ algItems } <- get
    let constants = filter (\case Constant{} -> True; _ -> False) algItems
    forM_ constants $ \Constant{ cX, cVar} ->
        addFunction Function{ fName="constant", fIn=[], fOut=[cVar], fValues=[cX] }



processStatement fn (LocalAssign names (Just [rexp])) = do
    processStatement fn $ LocalAssign names Nothing
    processStatement fn $ Assign (map VarName names) [rexp]

processStatement _fn (LocalAssign names Nothing) = do
    AlgBuilder{ algVars } <- get
    forM_ names $ \(Name n) ->
        when (n `elem` algVars) $ error "local variable alredy defined"

processStatement _fn st@(Assign lexps rexps)
    = if
        | length lexps == length rexps -> do
            let outs = map (\case (VarName (Name v)) -> [v]; l -> error $ "bad left expression: " ++ show l) lexps
            diff <- concat <$> mapM renameVarsIfNeeded outs
            zipWithM_ (rightExp diff) outs rexps
            flushBuffer
        | length lexps > 1 && length rexps == 1 -> do
            let outs = map (\case (VarName (Name v)) -> v; l -> error $ "bad left expression: " ++ show l) lexps
            diff <- renameVarsIfNeeded outs
            rightExp diff outs $ head rexps
            flushBuffer
        | otherwise -> error $ "processStatement: " ++ show st

processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
    | fn == fName
    = do
        AlgBuilder{ algItems } <- get
        let algIn = reverse $ filter (\case InputVar{} -> True; _ -> False) algItems
        mapM_ (uncurry f) $ zip algIn args
        where
            f InputVar{ iX, iVar } rexp = do
                i <- expArg [] rexp
                let fun = Function{ fName="loop", fIn=[i], fOut=[iVar], fValues=[iX] }
                modify' $ \alg@AlgBuilder{ algItems } -> alg{ algItems=fun : algItems }
            f _ _ = undefined

processStatement _fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (expArg []) args
    addFunction Function{ fName=unpack fName, fIn, fOut=[], fValues=[] }

processStatement _fn st = error $ "statement: " ++ show st



rightExp diff fOut (Binop op a b) = do
    a' <- expArg diff a
    b' <- expArg diff b
    let f = Function{ fName=binop op, fIn=[a', b'], fOut, fValues=[] }
    patchAndAddFunction f diff
    where
        binop Add = "add"
        binop Sub = "sub"
        binop Mul = "multiply"
        binop Div = "divide"
        binop o   = error $ "unknown binop: " ++ show o

rightExp
        diff
        fOut
        (PrefixExp (PEFunCall (NormalFunCall
            (PEVar (VarName (Name fn)))
            (Args args)
        ))) = do
    fIn <- mapM (expArg diff) args
    let f = Function{ fName=unpack fn, fIn, fOut, fValues=[] }
    patchAndAddFunction f diff

rightExp diff [a] (PrefixExp (PEVar (VarName (Name b)))) -- a = b
    = addItemToBuffer Alias{ aFrom=a, aTo=applyPatch diff b }

rightExp diff [a] n@(Number _ _) = do -- a = 42
    b <- expConstant (T.concat [a, "_constant"]) n
    addItemToBuffer Alias{ aFrom=a, aTo=applyPatch diff b }

rightExp _diff _out rexp = error $ "rightExp: " ++ show rexp



expArg _diff n@(Number _ _) = expConstant "constant" n

expArg _diff (PrefixExp (PEVar (VarName (Name var)))) = findAlias var

expArg diff call@(PrefixExp (PEFunCall _)) = do
    c <- genVar "tmp"
    rightExp diff [c] call
    return c

expArg diff (PrefixExp (Paren arg)) = expArg diff arg

expArg diff binop@Binop{} = do
    c <- genVar "tmp"
    rightExp diff [c] binop
    return c

expArg _diff a = error $ "expArg: " ++ show a



-- *Internal

expConstant prefix (Number IntNum textX) = do
    let x = read $ unpack textX
    AlgBuilder{ algItems } <- get
    case find (\case Constant{ cX } | cX == x -> True; _ -> False) algItems of
        Just Constant{ cVar } -> return cVar
        Nothing -> do
            g <- genVar prefix
            addItem Constant{ cX=x, cVar=g } []
            return g
        Just _ -> error "internal error"
expConstant _ _ = undefined



addFunction f@Function{ fOut } = do
    diff <- renameVarsIfNeeded fOut
    patchAndAddFunction f diff
addFunction e = error $ "addFunction try to add: " ++ show e



patchAndAddFunction f@Function{ fIn } diff = do
    let fIn' = map (applyPatch diff) fIn
    alg@AlgBuilder{ algItems } <- get
    put alg
        { algItems=f{ fIn=fIn' } : algItems
        }
patchAndAddFunction _ _ = undefined



renameVarsIfNeeded fOut = do
    AlgBuilder{ algVars } <- get
    mapM autoRename $ filter (`elem` algVars) fOut

autoRename var = do
    var' <- genVar var
    renameFromTo var var'
    return (var, var')

renameFromTo rFrom rTo = do
    alg@AlgBuilder{ algItems, algVars } <- get
    put alg
        { algItems=Renamed{ rFrom, rTo } : patch algItems
        , algVars=rTo : algVars
        }
    where
        patch [] = []
        patch (i@InputVar{ iVar } : xs) = i{ iVar=rn iVar } : patch xs
        patch (Constant x v : xs) = Constant x (rn v) : patch xs
        patch (Alias{ aFrom, aTo } : xs) = Alias (rn aFrom) (rn aTo) : patch xs
        patch (f@Function{ fIn, fOut } : xs) = f{ fIn=map rn fIn, fOut=map rn fOut } : patch xs
        patch (x:xs) = x : patch xs

        rn v
            | v == rFrom = rTo
            | otherwise = v



funAssignStatments (FunAssign _ (FunBody _ _ (Block statments _))) = statments
funAssignStatments _                                               = error "funAssignStatments : not function assignment"


flushBuffer = modify'
    $ \alg@AlgBuilder{ algBuffer, algItems } -> alg
        { algItems=algBuffer ++ algItems
        , algBuffer=[]
        }


addItemToBuffer item = modify'
    $ \alg@AlgBuilder{ algBuffer } -> alg
        { algBuffer=item : algBuffer
        }


addItem item vars = modify'
    $ \alg@AlgBuilder{ algItems, algVars } -> alg
        { algItems=item : algItems
        , algVars=vars ++ algVars
        }



genVar prefix = do
    alg@AlgBuilder{ algVarGen=g:gs } <- get
    put alg{ algVarGen=gs }
    return $ T.concat [prefix, g]



findAlias var0 = do
    AlgBuilder{ algItems } <- get
    return $ inner var0 algItems
    where
        inner var [] = var
        inner var (Alias{ aFrom, aTo }:xs)
            | aFrom == var = inner aTo xs
        inner var (_:xs) = inner var xs



applyPatch diff v
    = case find ((== v) . fst) diff of
        Just (_, v') -> v'
        _            -> v
