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
#if MIN_VERSION_template_haskell(2,9,0)
import Data.Maybe (catMaybes)
#endif /* MIN_VERSION_template_haskell(2,9,0) */

modName :: String
modName = "Language.Haskell.TH.Lift"

-- | Derive Lift instances for the given datatype.
deriveLift :: Name -> Q [Dec]
deriveLift = deriveLift' <=< reify

-- | Derive Lift instances for many datatypes.
deriveLiftMany :: [Name] -> Q [Dec]
deriveLiftMany = deriveLiftMany' <=< mapM reify

-- | Obtain Info values through a custom reification function. This is useful
-- when generating instances for datatypes that have not yet been declared.
deriveLift' :: Info -> Q [Dec]
deriveLift' = fmap (:[]) . deriveLiftOne

deriveLiftMany' :: [Info] -> Q [Dec]
deriveLiftMany' = mapM deriveLiftOne

-- | Generates a lambda expresson which behaves like 'lift' (without requiring
-- a 'Lift' instance).
makeLift :: Name -> Q Exp
makeLift = makeLift' <=< reify

-- | Like 'makeLift', but using a custom reification function.
makeLift' :: Info -> Q Exp
makeLift' i = withInfo i $ \_ n _ cons -> makeLiftOne n cons

deriveLiftOne :: Info -> Q Dec
deriveLiftOne i = withInfo i liftInstance
  where
    liftInstance dcx n vs cons = do
#if MIN_VERSION_template_haskell(2,9,0)
      roles <- qReifyRoles n
      -- Compute the set of phantom variables.
      let phvars = catMaybes $
            zipWith (\v role -> if role == PhantomR then Just v else Nothing)
                    vs
                    roles
#else /* MIN_VERSION_template_haskell(2,9,0) */
      let phvars = []
#endif
      instanceD (ctxt dcx phvars vs)
                (conT ''Lift `appT` typ n (map fst vs))
                [funD 'lift [clause [] (normalB (makeLiftOne n cons)) []]]
    typ n = foldl appT (conT n) . map varT
    -- Only consider *-kinded type variables, because Lift instances cannot
    -- meaningfully be given to types of other kinds. Further, filter out type
    -- variables that are obviously phantom.
    ctxt dcx phvars =
        fmap (dcx ++) . cxt . concatMap liftPred . filter (`notElem` phvars)
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
        e = foldl (\e1 e2 -> [| appE $e1 $e2 |]) con args
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
#if MIN_VERSION_template_haskell(2,4,0)
         -> (Cxt -> Name -> [(Name, Kind)] -> [Con] -> Q a)
#else /* !(MIN_VERSION_template_haskell(2,4,0)) */
         -> (Cxt -> Name -> [Name]         -> [Con] -> Q a)
#endif
         -> Q a
withInfo i f = case i of
    TyConI (DataD dcx n vsk cons _) ->
        f dcx n (map unTyVarBndr vsk) cons
    TyConI (NewtypeD dcx n vsk con _) ->
        f dcx n (map unTyVarBndr vsk) [con]
    _ -> error (modName ++ ".deriveLift: unhandled: " ++ pprint i)
  where
#if MIN_VERSION_template_haskell(2,8,0)
    unTyVarBndr (PlainTV v) = (v, StarT)
    unTyVarBndr (KindedTV v k) = (v, k)
#elif MIN_VERSION_template_haskell(2,4,0)
    unTyVarBndr (PlainTV v) = (v, StarK)
    unTyVarBndr (KindedTV v k) = (v, k)
#else /* !(MIN_VERSION_template_haskell(2,4,0)) */
    unTyVarBndr :: Name -> Name
    unTyVarBndr v = v
#endif

-- A type-restricted version of error that ensures makeLift always returns a
-- value of type Q Exp, even when used on an empty datatype.
errorQExp :: String -> Q Exp
errorQExp = error
{-# INLINE errorQExp #-}

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
