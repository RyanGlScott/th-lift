{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.TH.Lift
  ( deriveLift
  , deriveLiftMany
  , deriveLift'
  , deriveLiftMany'
  , makeLift
  , makeLift'
  , Lift(..)
  ) where

#if !(MIN_VERSION_template_haskell(2,4,0))
import Data.PackedString (PackedString, packString, unpackPS)
#endif /* MIN_VERSION_template_haskell(2,4,0) */

import GHC.Base (unpackCString#)
import GHC.Exts (Double(..), Float(..), Int(..), Word(..))
import GHC.Prim (Addr#, Double#, Float#, Int#, Word#)
#if MIN_VERSION_template_haskell(2,11,0)
import GHC.Exts (Char(..))
import GHC.Prim (Char#)
#endif /* !(MIN_VERSION_template_haskell(2,11,0)) */

#if MIN_VERSION_template_haskell(2,8,0)
import Data.Char (ord)
#endif /* !(MIN_VERSION_template_haskell(2,8,0)) */
#if !(MIN_VERSION_template_haskell(2,10,0))
import Data.Ratio (Ratio)
#endif /* !(MIN_VERSION_template_haskell(2,10,0)) */
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad ((<=<), zipWithM)
import Data.List (foldl')
#if MIN_VERSION_template_haskell(2,7,0)
import Data.List (find)
#endif /* !(MIN_VERSION_template_haskell(2,7,0)) */
#if MIN_VERSION_template_haskell(2,9,0)
import Data.Maybe (catMaybes)
#endif /* MIN_VERSION_template_haskell(2,9,0) */
#if MIN_VERSION_template_haskell(2,8,0) && __GLASGOW_HASKELL__ < 710
import qualified Data.Set as Set (Set, empty, singleton, size, union, unions)
#endif /* !(MIN_VERSION_template_haskell(2,8,0)) || __GLASGOW_HASKELL__ >= 710 */

modName :: String
modName = "Language.Haskell.TH.Lift"

-- | Derive 'Lift' instances for the given datatype.
-- For plain datatypes, simply give the name of the type constructor
-- (preceding the name with two single quotes):
--
-- @
-- data Pair a = MkPair a a
--
-- $(deriveLift ''Pair)
-- @
--
-- If using GHC 7.4 or later, you can also derive 'Lift' for data family instances.
-- To do so, pass the name of a data family instance constructor
-- (preceding the name with one single quote):
--
-- @
-- data family PairFam a
-- data instance PairFam a = IntPair Int Int
--                         | OtherPair a a
--
-- $(deriveLift 'IntPair)
-- -- Or, equivalently, $(deriveLift 'OtherPair)
-- @
deriveLift :: Name -> Q [Dec]
deriveLift = deriveLift' <=< reify

-- | Derive 'Lift' instances for many datatypes.
deriveLiftMany :: [Name] -> Q [Dec]
deriveLiftMany = deriveLiftMany' <=< mapM reify

-- | Obtain 'Info' values through a custom reification function. This is useful
-- when generating instances for datatypes that have not yet been declared.
deriveLift' :: Info -> Q [Dec]
deriveLift' = fmap (:[]) . deriveLiftOne

deriveLiftMany' :: [Info] -> Q [Dec]
deriveLiftMany' = mapM deriveLiftOne

-- | Generates a lambda expresson which behaves like 'lift' (without requiring
-- a 'Lift' instance). Example:
--
-- @
-- newtype Fix f = In { out :: f (Fix f) }
--
-- instance Lift (f (Fix f)) => Lift (Fix f) where
--   lift = $(makeLift ''Fix)
-- @
makeLift :: Name -> Q Exp
makeLift = makeLift' <=< reify

-- | Like 'makeLift', but using a custom reification function.
makeLift' :: Info -> Q Exp
makeLift' i = withInfo i $ \_ n _ cons _ -> makeLiftOne n cons

deriveLiftOne :: Info -> Q Dec
deriveLiftOne i = withInfo i liftInstance
  where
    liftInstance :: Cxt -> Name -> [(Name, Kind)] -> [Con] -> Maybe [Type] -> Q Dec
    liftInstance dcx n vsk cons mbTys = do
      (instanceCxt, instanceType) <- buildTypeInstance dcx n vsk mbTys
      instanceD instanceCxt
                instanceType
                [funD 'lift [clause [] (normalB (makeLiftOne n cons)) []]]

makeLiftOne :: Name -> [Con] -> Q Exp
makeLiftOne n cons = do
  e <- newName "e"
  lam1E (varP e) $ caseE (varE e) $ consMatches n cons

consMatches :: Name -> [Con] -> [Q Match]
consMatches n [] = [match wildP (normalB e) []]
  where
    e = [| errorQExp $(stringE ("Can't lift value of empty datatype " ++ nameBase n)) |]
consMatches _ cons = map doCons cons

doCons :: Con -> Q Match
doCons (NormalC c sts) = do
    ns <- zipWithM (\_ i -> newName ('x':show (i :: Int))) sts [0..]
    let con = [| conE c |]
        args = [ liftVar n t | (n, (_, t)) <- zip ns sts ]
        e = foldl' (\e1 e2 -> [| appE $e1 $e2 |]) con args
    match (conP c (map varP ns)) (normalB e) []
doCons (RecC c sts) = doCons $ NormalC c [(s, t) | (_, s, t) <- sts]
doCons (InfixC sty1 c sty2) = do
    x0 <- newName "x0"
    x1 <- newName "x1"
    let con = [| conE c |]
        left = liftVar x0 (snd sty1)
        right = liftVar x1 (snd sty2)
        e = [| infixApp $left $con $right |]
    match (infixP (varP x0) c (varP x1)) (normalB e) []
doCons (ForallC _ _ c) = doCons c

liftVar :: Name -> Type -> Q Exp
liftVar varName (ConT tyName)
#if MIN_VERSION_template_haskell(2,8,0)
  | tyName == ''Addr#   = [| litE (stringPrimL (map (fromIntegral . ord)
                                                    (unpackCString# $var))) |]
#else /* !(MIN_VERSION_template_haskell(2,8,0)) */
  | tyName == ''Addr#   = [| litE (stringPrimL (unpackCString# $var))       |]
#endif
#if MIN_VERSION_template_haskell(2,11,0)
  | tyName == ''Char#   = [| litE (charPrimL               (C# $var))  |]
#endif  /* !(MIN_VERSION_template_haskell(2,11,0)) */
  | tyName == ''Double# = [| litE (doublePrimL (toRational (D# $var))) |]
  | tyName == ''Float#  = [| litE (floatPrimL  (toRational (F# $var))) |]
  | tyName == ''Int#    = [| litE (intPrimL    (toInteger  (I# $var))) |]
  | tyName == ''Word#   = [| litE (wordPrimL   (toInteger  (W# $var))) |]
  where
    var :: Q Exp
    var = varE varName
liftVar varName _ = [| lift $(varE varName) |]

withInfo :: Info
         -> (Cxt -> Name -> [(Name, Kind)] -> [Con] -> Maybe [Type] -> Q a)
         -> Q a
withInfo i f = case i of
    TyConI (DataD dcx n vsk cons _) ->
        f dcx n (map unTyVarBndr vsk) cons Nothing
    TyConI (NewtypeD dcx n vsk con _) ->
        f dcx n (map unTyVarBndr vsk) [con] Nothing
#if MIN_VERSION_template_haskell(2,7,0)
# if MIN_VERSION_template_haskell(2,11,0)
    DataConI instName _ parentName   -> do
# else
    DataConI instName _ parentName _ -> do
# endif
      parentInfo <- reify parentName
      case parentInfo of
# if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (DataFamilyD _ vsk _) decs ->
# else
        FamilyI (FamilyD DataFam _ vsk _) decs ->
# endif
          let getConName :: Con -> Name
              getConName (NormalC name _)  = name
              getConName (RecC name _)     = name
              getConName (InfixC _ name _) = name
              getConName (ForallC _ _ con) = getConName con

              instDec :: Maybe Dec
              instDec = flip find decs $ \dec -> case dec of
                DataInstD    _ _ _ cons _ -> any ((instName ==) . getConName) cons
                NewtypeInstD _ _ _ con  _ -> instName == getConName con
                _ -> error $ prefix ++ "Must be a data or newtype instance."
          in case instDec of
            Just (DataInstD    dcx _ instTys cons _) ->
                f dcx parentName (map unTyVarBndr vsk) cons $ Just instTys
            Just (NewtypeInstD dcx _ instTys con  _) ->
                f dcx parentName (map unTyVarBndr vsk) [con] $ Just instTys
            _ -> error $ prefix ++
                    "Could not find data or newtype instance constructor."
        _ -> error $ prefix ++ "Data constructor " ++ show instName ++
               " is not from a data family instance constructor."
# if MIN_VERSION_template_haskell(2,11,0)
    FamilyI DataFamilyD{} _ ->
# else
    FamilyI (FamilyD DataFam _ _ _) _ ->
# endif
      error $ prefix ++
        "Cannot use a data family name. Use a data family instance constructor instead."
#endif
    _ -> error (prefix ++ "unhandled: " ++ pprint i)
  where
    prefix = modName ++ ".deriveLift: "

-- | Infer the context and instance head needed for a Lift instance.
buildTypeInstance :: Cxt
                  -- ^ The datatype context
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [(Name, Kind)]
                  -- ^ The type variables from the data type/data family declaration
                  -> Maybe [Type]
                  -- ^ 'Just' the types used to instantiate a data family instance,
                  -- or 'Nothing' if it's a plain data type
                  -> Q (Q Cxt, Q Type)
                  -- ^ The resulting 'Cxt' and 'Type' to use in a class instance
-- Plain data type/newtype case
buildTypeInstance dcx tyConName vsk Nothing = do
    phvars <- computePhvars tyConName vsk
    return (ctxt dcx phvars vsk, conT ''Lift `appT` instanceType)
  where
    instanceType :: Q Type
    instanceType = typ tyConName $ map (varT . fst) vsk
-- Data family instance case
buildTypeInstance dcx dataFamName vsk (Just instTysAndKinds) = do
    phvars <- computePhvars dataFamName vsk
    return (ctxt dcx phvars lhsVs, conT ''Lift `appT` instanceType)
  where
    -- We need to make sure that type variables in the instance head which have
    -- constraints aren't poly-kinded, e.g.,
    --
    -- @
    -- instance Lift a => Lift (Foo (a :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Q Type
    instanceType = typ dataFamName $ map (return . unSigT) rhsTypes

    -- We need to mindful of an old GHC bug which causes kind variables appear in
    -- @instTysAndKinds@ (as the name suggests) if (1) @PolyKinds@ is enabled, and
    -- (2) either GHC 7.6 or 7.8 is being used (for more info, see
    -- https://ghc.haskell.org/trac/ghc/ticket/9692).
    --
    -- Since Template Haskell doesn't seem to have a mechanism for detecting which
    -- language extensions are enabled, we do the next-best thing by counting
    -- the number of distinct kind variables in the data family declaration, and
    -- then dropping that number of entries from @instTysAndKinds@
    instTypes :: [Type]
    instTypes =
#if __GLASGOW_HASKELL__ >= 710 || !(MIN_VERSION_template_haskell(2,8,0))
        instTysAndKinds
#else
        drop (Set.size . Set.unions $ map (distinctKindVars . snd) vsk)
             instTysAndKinds

    distinctKindVars :: Kind -> Set.Set Name
    distinctKindVars (AppT k1 k2) = distinctKindVars k1 `Set.union` distinctKindVars k2
    distinctKindVars (SigT k _)   = distinctKindVars k
    distinctKindVars (VarT k)     = Set.singleton k
    distinctKindVars _            = Set.empty
#endif

    lhsVs :: [(Name, Kind)]
    lhsVs = map (uncurry replaceTyVarName)
          . filter (isTyVar . snd)
          $ zip vsk rhsTypes

    -- In GHC 7.8, only the @Type@s up to the rightmost non-eta-reduced type variable
    -- in @instTypes@ are provided (as a result of this bug:
    -- https://ghc.haskell.org/trac/ghc/ticket/9692). To work around this, we borrow
    -- some type variables from the data family instance declaration.
    rhsTypes :: [Type]
    rhsTypes =
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
            instTypes ++ map (\(n, k) -> SigT (VarT n) k)
                             (drop (length instTypes) vsk)
#else
            instTypes
#endif

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Replace the Name of a type variable with one from a Type (if the Type has a Name).
replaceTyVarName :: (Name, Kind) -> Type -> (Name, Kind)
replaceTyVarName nk     (SigT t _) = replaceTyVarName nk t
replaceTyVarName (_, k) (VarT n)   = (n, k)
replaceTyVarName nk     _          = nk

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- A type-restricted version of error that ensures makeLift always returns a
-- value of type Q Exp, even when used on an empty datatype.
errorQExp :: String -> Q Exp
errorQExp = error
{-# INLINE errorQExp #-}

typ :: Name -> [Q Type] -> Q Type
typ n = foldl' appT (conT n)

-- Only consider *-kinded type variables, because Lift instances cannot
-- meaningfully be given to types of other kinds. Further, filter out type
-- variables that are obviously phantom.
ctxt :: [Pred] -> [(Name, Kind)] -> [(Name, Kind)] -> Q Cxt
ctxt dcx phvars =
    fmap (dcx ++) . cxt . concatMap liftPred . filter (`notElem` phvars)
  where
#if MIN_VERSION_template_haskell(2,10,0)
    liftPred (v, StarT) = [conT ''Lift `appT` varT v]
    liftPred (_, _) = []
#elif MIN_VERSION_template_haskell(2,8,0)
    liftPred (v, StarT) = [classP ''Lift [varT v]]
    liftPred (_, _) = []
#elif MIN_VERSION_template_haskell(2,4,0)
    liftPred (v, StarK) = [classP ''Lift [varT v]]
    liftPred (_, _) = []
#else /* !(MIN_VERSION_template_haskell(2,4,0)) */
    liftPred n = conT ''Lift `appT` varT n
#endif

computePhvars :: Name -> [(Name, Kind)] -> Q [(Name, Kind)]
#if MIN_VERSION_template_haskell(2,9,0)
computePhvars n vsk = do
    roles <- qReifyRoles n
    -- Compute the set of phantom variables.
    return $ catMaybes $
        zipWith (\v role -> if role == PhantomR then Just v else Nothing)
                vsk
                roles
#else /* MIN_VERSION_template_haskell(2,9,0) */
computePhvars _ _ = return []
#endif

unTyVarBndr :: TyVarBndr -> (Name, Kind)
#if MIN_VERSION_template_haskell(2,8,0)
unTyVarBndr (PlainTV v) = (v, StarT)
unTyVarBndr (KindedTV v k) = (v, k)
#elif MIN_VERSION_template_haskell(2,4,0)
unTyVarBndr (PlainTV v) = (v, StarK)
unTyVarBndr (KindedTV v k) = (v, k)
#else /* !(MIN_VERSION_template_haskell(2,4,0)) */
unTyVarBndr v = v
#endif

instance Lift Name where
  lift (Name occName nameFlavour) = [| Name occName nameFlavour |]

#if MIN_VERSION_template_haskell(2,4,0)
instance Lift OccName where
  lift n = [| mkOccName $(lift $ occString n) |]

instance Lift PkgName where
  lift n = [| mkPkgName $(lift $ pkgString n) |]

instance Lift ModName where
  lift n = [| mkModName $(lift $ modString n) |]

#else /* MIN_VERSION_template_haskell(2,4,0) */
instance Lift PackedString where
  lift ps = [| packString $(lift $ unpackPS ps) |]

#endif /* MIN_VERSION_template_haskell(2,4,0) */
instance Lift NameFlavour where
  lift NameS = [| NameS |]
  lift (NameQ modnam) = [| NameQ modnam |]
#if __GLASGOW_HASKELL__ >= 710
  lift (NameU i) = [| NameU i |]
  lift (NameL i) = [| NameL i |]
#else /* __GLASGOW_HASKELL__ < 710 */
  lift (NameU i) = [| case $( lift (I# i) ) of
                          I# i' -> NameU i' |]
  lift (NameL i) = [| case $( lift (I# i) ) of
                          I# i' -> NameL i' |]
#endif /* __GLASGOW_HASKELL__ < 710 */
  lift (NameG nameSpace pkgName modnam)
   = [| NameG nameSpace pkgName modnam |]

instance Lift NameSpace where
  lift VarName = [| VarName |]
  lift DataName = [| DataName |]
  lift TcClsName = [| TcClsName |]

#if !(MIN_VERSION_template_haskell(2,10,0))
-- These instances should really go in the template-haskell package.

instance Lift () where
  lift _ = [| () |]

instance Integral a => Lift (Ratio a) where
  lift x = return (LitE (RationalL (toRational x)))
#endif
