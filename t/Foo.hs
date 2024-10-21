{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE UnliftedNewtypes #-}
#endif
module Foo where

import GHC.Prim (Char#, Double#, Float#, Int#, Word#)

import Language.Haskell.TH.Lift
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Lift.Internal (unsafeSpliceCoerce)
#endif

data (Eq a) => Foo a b = Foo a Char | Bar a
    deriving Show

newtype Rec a = Rec { field :: a }
                deriving Show

data Empty a

data Unboxed = Unboxed {
  primChar   :: Char#,
  primDouble :: Double#,
  primFloat  :: Float#,
  primInt    :: Int#,
  primWord   :: Word#
  } deriving Show

newtype Fix f = In { out :: f (Fix f) }
deriving instance Show (f (Fix f)) => Show (Fix f)

data family   Fam a b c
data instance Fam a Int Char
  = FamPrefix1 a Char
  | FamPrefix2 a
  | FamRec { famField :: a }
  | a :%%: a
  deriving Show
data instance Fam a Bool Bool = FamInstBool a Bool
  deriving Show

$(deriveLift ''Foo)
$(deriveLift ''Rec)
$(deriveLift ''Empty)
$(deriveLift ''Unboxed)
instance Lift (f (Fix f)) => Lift (Fix f) where
  lift = $(makeLift ''Fix)
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeSpliceCoerce . lift
#endif

$(deriveLift 'FamPrefix1)
instance (Eq a, Lift a) => Lift (Fam a Bool Bool) where
  lift = $(makeLift 'FamInstBool)
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeSpliceCoerce . lift
#endif

#if MIN_VERSION_template_haskell(2,16,0)
-- One can also implement Lift instances (on template-haskell-2.16+) by only
-- defining liftTyped and using the default definition of lift in terms of
-- liftTyped.
newtype Fix2 f = In2 { out2 :: f (Fix2 f) }
deriving instance Show (f (Fix2 f)) => Show (Fix2 f)

data family   Fam2 a b c
data instance Fam2 a Int Char
  = Fam2Prefix1 a Char
  | Fam2Prefix2 a
  | Fam2Rec { fam2Field :: a }
  | a :%%%: a
  deriving Show
data instance Fam2 a Bool Bool = Fam2InstBool a Bool
  deriving Show

$(pure [])

instance Lift (f (Fix2 f)) => Lift (Fix2 f) where
  liftTyped = unsafeSpliceCoerce . $(makeLift ''Fix2)
instance Lift a => Lift (Fam2 a Int Char) where
  liftTyped = unsafeSpliceCoerce . $(makeLift 'Fam2Prefix1)
instance (Eq a, Lift a) => Lift (Fam2 a Bool Bool) where
  liftTyped = unsafeSpliceCoerce . $(makeLift 'Fam2InstBool)
#endif

#if __GLASGOW_HASKELL__ >= 810
-- Regression test for #43
newtype T43 = MkT43 Int#
$(deriveLift ''T43)
#endif
