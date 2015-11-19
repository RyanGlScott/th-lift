{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_template_haskell(2,7,0)
{-# LANGUAGE TypeFamilies #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

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

$(deriveLift ''Foo)
$(deriveLift ''Rec)
$(deriveLift ''Empty)
$(deriveLift ''Unboxed)
instance Lift (f (Fix f)) => Lift (Fix f) where
  lift = $(makeLift ''Fix)


#if MIN_VERSION_template_haskell(2,7,0)
data family DataFam
# if __GLASGOW_HASKELL__ >= 706
                    (a :: k)
# else
                     a
# endif
data    instance DataFam Int  = FamFoo Int
                              | FamBar Int Int  deriving Show
newtype instance DataFam Char = FamNewtype Char deriving Show

$(deriveLift 'FamFoo)
instance Lift (DataFam Char) where
  lift = $(makeLift 'FamNewtype)
#endif
