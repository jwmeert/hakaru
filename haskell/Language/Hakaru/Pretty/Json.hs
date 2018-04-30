{-# LANGUAGE MultiParamTypeClasses
           , OverloadedStrings
           , FlexibleInstances
           , FlexibleContexts
           , ScopedTypeVariables
           , CPP
           , GADTs
           , TypeFamilies
           , DataKinds
           , TypeOperators
           #-}


{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2018.04.25
-- |
-- Module      :  Language.Hakaru.Pretty.Json
-- Copyright   :  Copyright (c) 2016 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  GHC-only
-- |
--
----------------------------------------------------------------
module Language.Hakaru.Pretty.Json (jsonType) where

import qualified Data.Text           as Text
import           Data.Ratio
import           Data.Number.Nat     (fromNat)
import           Data.Sequence       (Seq)
import qualified Data.Foldable       as F
import qualified Data.List.NonEmpty  as L
import           Control.Monad.State (MonadState(..), State, runState)
import           Data.Maybe          (isJust)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative   (Applicative(..), (<$>))
#endif

import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.Sing
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.Datum
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.IClasses
import Language.Hakaru.Expect

jsonType :: Sing (a :: Hakaru) -> ShowS
jsonType SNat = showString "\"Nat\""
jsonType SInt = showString "\"Int\""
jsonType SProb = showString "\"Prob\""
jsonType SReal = showString "\"Real\""
jsonType (SFun a b) = showString "{\"Function\" : " .
                      (jsonType a) .
                      (jsonType b) .
                      showString "}"
jsonType (SArray a) = showString "{\"Array\" : " .
                      (jsonType a) .
                      showString "}"
jsonType (SMeasure a) = showString "{\"Measure\" : " .
                        (jsonType a) .
                        showString "}"
-- Special case pair
jsonType (SData (STyCon c `STyApp` _ `STyApp` _) (SPlus x SVoid))
  | isJust (jmEq1 c sSymbol_Pair)
  = showString "{\"Pair\" : ["
  . jsonTypeDStruct x
  . showString "]}"

-- Special case unit
jsonType (SData (STyCon c) (SPlus SDone SVoid))
  | isJust (jmEq1 c sSymbol_Unit)
  = showString "{\"Unit\" : []}"
-- Special case Bool
jsonType (SData (STyCon c) (SPlus SDone (SPlus SDone SVoid)))
  | isJust (jmEq1 c sSymbol_Bool)
  = showString "\"Bool\""
jsonType x = error $ "TODO: jsonType{" ++ show x ++ "}"

dStructHelp :: Sing (x :: HakaruFun) -> Sing (xs :: [HakaruFun]) -> ShowS
dStructHelp x SDone = jsonTypeDFun x
dStructHelp x (SEt x' xs) =
  jsonTypeDFun x
  . showString ","
  . dStructHelp x' xs

jsonTypeDStruct :: Sing (a :: [HakaruFun]) -> ShowS
jsonTypeDStruct SDone = showString ""
jsonTypeDStruct (SEt x xs) = dStructHelp x xs

jsonTypeDFun :: Sing (a :: HakaruFun) -> ShowS
jsonTypeDFun (SKonst a) = (jsonType a)
jsonTypeDFun SIdent     = showString "Ident"

--
