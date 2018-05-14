{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Foo where

import GHC.Prim (Double#, Float#, Int#, Word#)
#if MIN_VERSION_template_haskell(2,11,0)
import GHC.Prim (Char#)
#endif

import Language.Haskell.TH.Lift

-- Phantom type parameters can't be dealt with poperly on GHC < 7.8.
#if MIN_VERSION_template_haskell(2,9,0)
data (Eq a) => Foo a b = Foo a Char | Bar a
#else
data (Eq a) => Foo a = Foo a Char | Bar a
#endif
    deriving Show

newtype Rec a = Rec { field :: a }
                deriving Show

data Empty a

data Unboxed = Unboxed {
-- Template Haskell couldn't handle unlifted chars on GHC < 8.0
#if MIN_VERSION_template_haskell(2,11,0)
  primChar   :: Char#,
#endif
  primDouble :: Double#,
  primFloat  :: Float#,
  primInt    :: Int#,
  primWord   :: Word#
  } deriving Show

newtype Fix f = In { out :: f (Fix f) }
deriving instance Show (f (Fix f)) => Show (Fix f)

#if MIN_VERSION_template_haskell(2,7,0)
data family   Fam a b c
data instance Fam a Int Char
  = FamPrefix1 a Char
  | FamPrefix2 a
  | FamRec { famField :: a }
  | a :%%: a
  deriving Show
data instance Fam a Bool Bool = FamInstBool a Bool
  deriving Show
#endif

$(deriveLift ''Foo)
$(deriveLift ''Rec)
$(deriveLift ''Empty)
$(deriveLift ''Unboxed)
instance Lift (f (Fix f)) => Lift (Fix f) where
  lift = $(makeLift ''Fix)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveLift 'FamPrefix1)
instance (Eq a, Lift a) => Lift (Fam a Bool Bool) where
  lift = $(makeLift 'FamInstBool)
#endif
