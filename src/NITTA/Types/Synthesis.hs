{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types.Synthesis
Description : Types to describe synthesis process
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Types.Synthesis
    ( SynthesisTree
    , Synthesis(..)
    , rootSynthesis
    , Nid(..)
    , nids
    , SynthUpd(..)
    , SubNode(..)
      -- *Processing SynthesisTree
    , getSynthesis
    , update
    , apply
    , recApply
      -- *Synhesis context
    , SynthCntxCls(..)
    , SynthCntx(..)
    , comment
    , setCntx
    , findCntx
    ) where

import           Data.List.Split
import           Data.Tree
import           Data.Typeable   (Typeable, cast)
import           GHC.Generics
import           NITTA.DataFlow  (SystemState)


type SynthesisTree title tag v x t = Tree (Synthesis title tag v x t)

data Synthesis title tag v x t
    = Synthesis
        { sModel :: SystemState title tag v x t
        , sCntx  :: [SynthCntx]
        }
    deriving ( Generic )


-- |Synthesis identical.
newtype Nid = Nid [Int]
nidSep = ':'

instance Show Nid where
    show (Nid []) = [nidSep]
    show (Nid is) = show' is
        where
            show' []     = ""
            show' (x:xs) = nidSep : show x ++ show' xs

instance Read Nid where
    readsPrec _ [x] | x == nidSep    = [(Nid [], "")]
    readsPrec d (x:xs)
        | x == nidSep
        , let is = map (readsPrec d) $ splitOn [nidSep] xs
        , all (not . null) is
        = [(Nid $ map fst $ concat is, "")]
    readsPrec _ _ = []


nids n = inner [] n
    where
        inner is Node{ subForest } = Node
            { rootLabel=Nid $ reverse is
            , subForest=zipWith (\i subN -> inner (i:is) subN) [0..] subForest
            }


-- |Create initial synthesis.
rootSynthesis m = Node
    { rootLabel=Synthesis
        { sModel=m
        , sCntx=[]
        }
    , subForest=[]
    }



-- *Synthesis context

class SynthCntxCls a where
    data SynthCntx' a :: *

data SynthCntx = forall a. ( Show (SynthCntx' a), Typeable (SynthCntx' a) ) => SynthCntx (SynthCntx' a)

instance Show SynthCntx where
    show (SynthCntx e) = show e

findCntx [] = Nothing
findCntx (SynthCntx c : cs)
    | Just cntx <- cast c = Just cntx
    | otherwise = findCntx cs

setCntx newCntx [] = [SynthCntx newCntx]
setCntx newCntx (SynthCntx c : cs)
    | Just c' <- cast c
    , let _ = c' `asTypeOf` newCntx
    = SynthCntx newCntx : cs
    | otherwise
    = SynthCntx c : setCntx newCntx cs


data Comment

instance SynthCntxCls Comment where
    data SynthCntx' Comment = Comment String
        deriving ( Show )

comment = SynthCntx . Comment



-- *Processing

-- |Update synthesis node data. Can affect subForest and current (or upper) node.
data SynthUpd a
    = SynthUpd
        { upp :: a -- ^new upper node
        , sub :: SubNode a -- ^new sub node
        }

data SubNode a
    = NewNode a
    | Patch Int

-- |Get specific by @nid@ node from a synthesis tree.
getSynthesis (Nid []) n                     = n
getSynthesis (Nid (i:is)) Node{ subForest } = getSynthesis (Nid is) (subForest !! i)


-- |Update specific by @nid@ node in a synthesis tree by the @f@.
update f nid rootN = inner nid rootN
    where
        inner (Nid []) n = f n
        inner (Nid (i:is)) n@Node{ subForest }
            = let
                (before, subN : after) = splitAt i subForest
            in case inner (Nid is) subN of
                Just (subN', Nid is') -> Just (n{ subForest=before ++ (subN' : after) }, Nid (i:is'))
                Nothing -> Nothing


-- |Recursively apply @rec@ to a synthesis while it is applicable (returning Just value).
recApply rec nRoot = inner nRoot
    where
        inner n@Node{ rootLabel, subForest }
            = case rec (length subForest) rootLabel of
                Just SynthUpd{ upp, sub=NewNode sub } -> -- rootLabel' ->
                    let (subN', Nid subIxs) = inner Node{ rootLabel=sub, subForest=[] }
                    in (n{ rootLabel=upp, subForest=subForest ++ [subN'] }, Nid (length subForest : subIxs) )
                Just SynthUpd{ upp, sub=Patch ix } ->
                    let (subN, Nid subIxs) = inner $ subForest !! ix
                    in (n{ rootLabel=upp, subForest=setSubNode ix subN subForest }, Nid (ix : subIxs))
                Nothing -> (n, Nid [])


-- |Apply @f@ to a synthesis in a node.
apply f n@Node{ rootLabel, subForest }
    = case f (length subForest) rootLabel of
        Just SynthUpd{ upp, sub=NewNode sub } ->
            let subN = Node
                    { rootLabel=sub
                    , subForest=[]
                    }
            in Just (n{ rootLabel=upp, subForest=subForest ++ [subN]}, Nid [length subForest])
        Just SynthUpd{ upp, sub=Patch ix } ->
            let subN = subForest !! ix
            in Just (n{ rootLabel=upp }, Nid [ix])
        Nothing -> Nothing


setSubNode i n forest
    | length forest == i = forest ++ [n]
    | let (before, subN : after) = splitAt i forest
    = before ++ (n : after)