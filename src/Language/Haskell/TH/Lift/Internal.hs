{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

-- | Helper functions used in code that "Language.Haskell.TH.Lift" generates.
--
-- Note: this is an internal module, and as such, the API presented here is not
-- guaranteed to be stable, even between minor releases of this library.
module Language.Haskell.TH.Lift.Internal where

#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Exts (RuntimeRep, TYPE)
#endif

import Language.Haskell.TH.Syntax

-- | A type-restricted version of 'error' that ensures 'makeLift' always
-- returns a value of type @q 'Exp'@ (where @q@ is an instance of 'Quote'),
-- even when used on an empty datatype.
#if MIN_VERSION_template_haskell(2,17,0)
errorQuoteExp :: Quote q => String -> q Exp
#else
errorQuoteExp ::            String -> Q Exp
#endif
errorQuoteExp = error

-- | This is a cargo-culted version of @unsafeSpliceCoerce@ from the
-- @th-compat@ library, which has been copied here to avoid incurring a library
-- dependency.
--
-- Only available when built with @template-haskell-2.9.0.0@ or later.
#if MIN_VERSION_template_haskell(2,17,0)
unsafeSpliceCoerce :: forall (r :: RuntimeRep) (a :: TYPE r) m. Quote m => m Exp -> Code m a
unsafeSpliceCoerce = unsafeCodeCoerce
#elif MIN_VERSION_template_haskell(2,16,0)
unsafeSpliceCoerce :: forall (r :: RuntimeRep) (a :: TYPE r). Q Exp -> Q (TExp a)
unsafeSpliceCoerce = unsafeTExpCoerce
#elif MIN_VERSION_template_haskell(2,9,0)
unsafeSpliceCoerce :: forall a. Q Exp -> Q (TExp a)
unsafeSpliceCoerce = unsafeTExpCoerce
#endif
