{-# LANGUAGE DataKinds,
             FlexibleContexts,
             GADTs,
             KindSignatures #-}

----------------------------------------------------------------
--                                                    2016.07.11
-- |
-- Module      :  Language.Hakaru.CodeGen.Declaration
-- Copyright   :  Copyright (c) 2016 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  zsulliva@indiana.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Provides tools for building C declarations from Hakaru types
--
----------------------------------------------------------------

module Language.Hakaru.CodeGen.HOAS.Declaration
  ( -- tools for building C types
    typeDeclaration
  , arrayDeclaration
  , structDeclaration
  ) where

import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Syntax.AST

import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.Datum
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.Sing

node :: NodeInfo
node = undefNode

typeDeclaration :: Sing (a :: Hakaru) -> Ident -> CDecl
typeDeclaration typ ident =
  CDecl [CTypeSpec $ buildType typ]
        [(Just $ CDeclr (Just ident) [] Nothing [] node,Nothing,Nothing)]
        node

arrayDeclaration :: Sing (a :: Hakaru)
                 -> CExpr -- ^ cexpr representing arity (could lead to bugs?)
                 -> Ident
                 -> CDecl
arrayDeclaration typ n ident =
  CDecl [CTypeSpec $ buildType typ]
        [( Just $ CDeclr (Just ident)
                         [CArrDeclr [] (CArrSize False n) node]
                         Nothing
                         []
                         node
         , Nothing
         , Nothing)]
        node

structDeclaration :: (ABT Term abt)
                  => DatumCode (Code t) (abt '[]) (HData' t)
                  -> Ident
                  -> CDecl
structDeclaration dcode ident =
  case dcode of
    (Inr _) -> struct
    (Inl _) -> error $ "TODO: structDeclaration: Inl"
  where struct = CDecl [CTypeSpec $ CSUType (CStruct CStructTag Nothing (Just []) [] node) node]
                       [( Just $ CDeclr (Just ident)
                                        []
                                        Nothing
                                        []
                                        node
                        , Nothing
                        , Nothing)]
                       node


----------------------------------------------------------------
-- | buildType function do the work of describing how the Hakaru
-- type will be stored in memory. Arrays needed their own
-- declaration function for their arity
buildType :: Sing (a :: Hakaru) -> CTypeSpec
buildType SInt         = CIntType undefNode
buildType SNat         = CIntType undefNode
buildType SProb        = CDoubleType undefNode
buildType SReal        = CDoubleType undefNode
buildType (SMeasure x) = buildType x
buildType (SData _ d)  = buildData d
buildType (SArray x)   = buildType x
buildType x = error $ "TODO: buildCType: " ++ show x

buildData :: Sing (b :: [[HakaruFun]])
          -> CTypeSpec
buildData d = error $ show d