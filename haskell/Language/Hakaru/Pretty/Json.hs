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


jop1 :: String -> ShowS -> ShowS
jop1 fn x = showString fn . curlies x
{-# INLINE jop1 #-}

jop2 :: String -> ShowS -> ShowS -> ShowS
jop2 fn x y = showString fn . curlies (x . showString ", " . y)
{-# INLINE jop2 #-}

jop3 :: String -> ShowS -> ShowS -> ShowS -> ShowS
jop3 fn x y z
  = showString fn
    . curlies
      ( x
      . showString ", "
      . y
      . showString ", "
      . z
      )
{-# INLINE jop3 #-}

curlies :: ShowS -> ShowS
curlies a = showChar '{' . a . showChar '}'
{-# INLINE curlies #-}

jsonType :: Sing (a :: Hakaru) -> ShowS
jsonType SNat = showString "\"Nat\" : \"Nat\""
jsonType SInt = showString "\"Int\" : \"Int\""
jsonType SProb = showString "\"Prob\" : \"Prob\""
jsonType SReal = showString "\"Real\" : \"Real\""
jsonType (SFun a b) = jop2 "\"Function\" : " (jsonType a) (jsonType b)
jsonType (SArray a) = jop1 "\"Array\" : " (jsonType a)
jsonType (SMeasure a) = jop1 "\"Measure\" : " (jsonType a)
-- Special case pair
jsonType (SData (STyCon c `STyApp` _ `STyApp` _) (SPlus x SVoid))
  | isJust (jmEq1 c sSymbol_Pair)
  = showString "\"Pair\" : ["
  . jsonTypeDStruct x
  . showString "]"
-- Special case unit
jsonType (SData (STyCon c) (SPlus SDone SVoid))
  | isJust (jmEq1 c sSymbol_Unit)
  = showString "\"Unit\" : []"
-- Special case Bool
jsonType (SData (STyCon c) (SPlus SDone (SPlus SDone SVoid)))
  | isJust (jmEq1 c sSymbol_Bool)
  = showString "\"Bool\""
jsonType x = error $ "TODO: jsonType{" ++ show x ++ "}"

jsonTypeDStruct :: Sing (a :: [HakaruFun]) -> ShowS
jsonTypeDStruct SDone = showString "\"NULL\" : \"NULL\""
jsonTypeDStruct (SEt x xs) =
  jsonTypeDFun x
  . showString ","
  . jsonTypeDStruct xs

jsonTypeDFun :: Sing (a :: HakaruFun) -> ShowS
jsonTypeDFun (SKonst a) = jop1 "" (jsonType a)
jsonTypeDFun SIdent     = showString "Ident"

--
