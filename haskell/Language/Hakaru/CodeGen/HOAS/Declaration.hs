{-# LANGUAGE DataKinds,
             FlexibleContexts,
             GADTs,
             KindSignatures #-}

----------------------------------------------------------------
--                                                    2016.07.11
-- |
-- Module      :  Language.Hakaru.CodeGen.HOAS.Declaration
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
  ( buildDeclaration

  -- tools for building C types
  , typeDeclaration
  , typePtrDeclaration
  , arrayDeclaration
  , arrayName
  , arrayStruct
  , datumDeclaration
  , datumName
  , datumStruct
  , functionDef

  , datumSum
  , datumProd

  , buildType
  , mkDecl
  , mkPtrDecl
  , buildStruct
  , buildUnion

  , doubleTyp
  , intTyp
  , intDecl
  , doubleDecl
  , doublePtr
  , intPtr
  , boolTyp
  ) where

import Control.Monad.State

import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.Sing
import Language.Hakaru.CodeGen.AST

buildDeclaration :: CTypeSpec -> Ident -> CDecl
buildDeclaration ctyp ident =
  CDecl [CTypeSpec ctyp]
        [( Just (CDeclr (Just ident) [] Nothing [])
         , Nothing
         , Nothing)]

typeDeclaration :: Sing (a :: Hakaru) -> Ident -> CDecl
typeDeclaration typ ident =
  CDecl [CTypeSpec $ buildType typ]
        [( Just (CDeclr (Just ident) [] Nothing [])
         , Nothing
         , Nothing)]

typePtrDeclaration :: Sing (a :: Hakaru) -> Ident -> CDecl
typePtrDeclaration typ ident =
  CDecl [CTypeSpec $ buildType typ]
        [( Just (CDeclr (Just ident) [CPtrDeclr []] Nothing [])
         , Nothing
         , Nothing)]

--------------------------------------------------------------------------------
--

arrayName :: Sing (a :: Hakaru) -> String
arrayName SInt  = "arrayInt"
arrayName SNat  = "arrayNat"
arrayName SReal = "arrayReal"
arrayName SProb = "arrayProb"
arrayName t    = error $ "arrayName: cannot make array from type: " ++ show t

arrayStruct :: Sing (a :: Hakaru) -> CExtDecl
arrayStruct t = CDeclExt (CDecl [CTypeSpec $ arrayStruct' t] [])

arrayStruct' :: Sing (a :: Hakaru) -> CTypeSpec
arrayStruct' t = aStruct
  where aSize   = CDecl [CTypeSpec intTyp ]
                        [( Just (CDeclr (Just (Ident "size")) [] Nothing [])
                         , Nothing
                         , Nothing)]
        aData   = CDecl [CTypeSpec $ buildType t]
                        [( Just (CDeclr (Just (Ident "data")) [CPtrDeclr []] Nothing [])
                         , Nothing
                         , Nothing)]
        aStruct = buildStruct (Just . Ident . arrayName $ t) [aSize,aData]


arrayDeclaration
  :: Sing (a :: Hakaru)
  -> Ident
  -> CDecl
arrayDeclaration typ = buildDeclaration (callStruct (arrayName typ))

--------------------------------------------------------------------------------
-- | datumProd and datumSum use a store of names, which needs to match up with
-- the names used when they are assigned and printed
-- datumDeclaration declares struct internally
-- datumStruct declares struct definitions externally

-- | datumName provides a unique name to identify a struct type
datumName :: Sing (a :: [[HakaruFun]]) -> String
datumName SVoid = "V"
datumName (SPlus prodD sumD) = concat ["S",datumName' prodD,datumName sumD]
  where datumName' :: Sing (a :: [HakaruFun]) -> String
        datumName' SDone = "U"
        datumName' (SEt (SKonst x) prod') = concat ["S",tail . show $ x,datumName' prod']
        datumName' (SEt SIdent _)         = error "TODO: datumName of SIdent"

datumNames :: [String]
datumNames = filter (\n -> not $ elem (head n) ['0'..'9']) names
  where base = ['0'..'9'] ++ ['a'..'z']
        names = [[x] | x <- base] `mplus` (do n <- names
                                              [n++[x] | x <- base])

datumStruct :: (Sing (HData' t)) -> CExtDecl
datumStruct (SData _ typ) = CDeclExt $ datumSum typ (Ident (datumName typ))

datumDeclaration
  :: (Sing (HData' t))
  -> Ident
  -> CDecl
datumDeclaration (SData _ typ) = buildDeclaration (callStruct (datumName typ))

datumSum :: Sing (a :: [[HakaruFun]]) -> Ident -> CDecl
datumSum funs ident =
  let declrs = fst $ runState (datumSum' funs) datumNames
      union  = CDecl [ CTypeSpec . buildUnion $ declrs ]
                     [ ( Just (CDeclr (Just (Ident "sum")) [] Nothing [])
                       , Nothing
                       , Nothing)]
      index  = CDecl [ CTypeSpec intTyp ]
                     [ ( Just (CDeclr (Just (Ident "index")) [] Nothing [])
                       , Nothing
                       , Nothing)]
      struct = buildStruct (Just ident) $ case declrs of
                                            [] -> [index]
                                            _  -> [index,union]
  in CDecl [ CTypeSpec struct ]
           [ ( Just (CDeclr Nothing [] Nothing [])
             , Nothing
             , Nothing)]

datumSum' :: Sing (a :: [[HakaruFun]]) -> State [String] [CDecl]
datumSum' SVoid          = return []
datumSum' (SPlus prod rest) =
  do (name:names) <- get
     put names
     let ident = internalIdent name
         mdecl = datumProd prod ident
     rest' <- datumSum' rest
     case mdecl of
       Nothing -> return rest'
       Just d  -> return $ [d] ++ rest'


datumProd :: Sing (a :: [HakaruFun]) -> Ident -> Maybe CDecl
datumProd SDone _     = Nothing
datumProd funs ident  =
  let declrs = fst $ runState (datumProd' funs) datumNames
  in  Just $ CDecl [ CTypeSpec . buildStruct Nothing $ declrs ]
                   [ ( Just (CDeclr (Just ident) [] Nothing [])
                     , Nothing
                     , Nothing)]

-- datumProd uses a store of names, which needs to match up with the names used
-- when they are assigned as well as printed
datumProd' :: Sing (a :: [HakaruFun]) -> State [String] [CDecl]
datumProd' SDone                 = return []
datumProd' (SEt (SKonst t) rest) =
  do (name:names) <- get
     put names
     let ident = Ident name
         decl  = CDecl [ CTypeSpec . buildType $ t ]
                       [ ( Just (CDeclr (Just ident) [] Nothing [])
                         , Nothing
                         , Nothing)]
     rest' <- datumProd' rest
     return $ [decl] ++ rest'
datumProd' (SEt SIdent _) = error "TODO: datumProd' SIdent"

----------------------------------------------------------------

functionDef
  :: Sing (a :: Hakaru)
  -> Ident
  -> [CDecl]
  -> [CDecl]
  -> [CStat]
  -> CFunDef
functionDef typ ident argDecls internalDecls stmts =
  CFunDef [CTypeSpec (buildType typ)]
          (CDeclr (Just ident) [CFunDeclr (Right (argDecls,False)) []] Nothing [])
          []
          (CCompound [] ((fmap CBlockDecl internalDecls) ++ (fmap CBlockStmt stmts)))

----------------------------------------------------------------
-- | buildType function do the work of describing how the Hakaru
-- type will be stored in memory. Arrays needed their own
-- declaration function for their arity
buildType :: Sing (a :: Hakaru) -> CTypeSpec
buildType SInt         = CInt
buildType SNat         = CInt
buildType SProb        = CDouble
buildType SReal        = CDouble
buildType (SMeasure x) = buildType x
buildType (SArray t)   = callStruct $ arrayName t
buildType (SFun _ x)   = buildType x -- build type the function returns
buildType (SData _ t)  = callStruct $ datumName t


-- these mk...Decl functions are used in coersions
mkDecl :: CTypeSpec -> CDecl
mkDecl t = CDecl [CTypeSpec t]
                 [( Nothing
                  , Nothing
                  , Nothing)]

mkPtrDecl :: CTypeSpec -> CDecl
mkPtrDecl t = CDecl [CTypeSpec t]
                    [( Just (CDeclr Nothing [CPtrDeclr []] Nothing [])
                     , Nothing
                     , Nothing)]

buildStruct :: Maybe Ident -> [CDecl] -> CTypeSpec
buildStruct mi [] =
  CSUType (CStruct CStructTag mi Nothing [])
buildStruct mi declrs =
  CSUType (CStruct CStructTag mi (Just declrs) [])

-- | callStruct will give the type spec calling a struct we have already declared externally
callStruct :: String -> CTypeSpec
callStruct name =
  CSUType (CStruct CStructTag (Just (Ident name)) Nothing)

buildUnion :: [CDecl] -> CTypeSpec
buildUnion [] =
 CSUType (CStruct CUnionTag Nothing Nothing)
buildUnion declrs =
 CSUType (CStruct CUnionTag Nothing (Just declrs))



intTyp,doubleTyp :: CTypeSpec
intTyp    = CInt
doubleTyp = CDouble


intDecl,doubleDecl :: CDecl
intDecl    = mkDecl intTyp
doubleDecl = mkDecl doubleTyp

intPtr,doublePtr :: CDecl
intPtr    = mkPtrDecl intTyp
doublePtr = mkPtrDecl doubleTyp

boolTyp :: CDecl
boolTyp =
  CDecl [CTypeSpec
          (CSUType
            (CStruct CStructTag
                     (Just (Ident "bool"))
                     (Just [CDecl [CTypeSpec CInt]
                                  [( Just (CDeclr (Just (Ident "index")) [] Nothing [])
                                   , Nothing
                                   , Nothing)]])))]
        []
