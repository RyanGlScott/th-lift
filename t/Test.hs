{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Foo
import Language.Haskell.TH.Syntax

main :: IO ()
main = do print $( lift (Foo "str1" 'c') )
          print $( lift (Bar "str2") )
          print $( lift (Rec {field = 'a'}) )
          print $( lift (Unboxed
#if MIN_VERSION_template_haskell(2,11,0)
                          'a'#
#endif
                          1.0## 1.0# 1# 1##) )
          print $( lift (In { out = Nothing }) )
#if MIN_VERSION_template_haskell(2,7,0)
          print $( lift (FamPrefix1 "str1" 'c') )
          print $( lift (FamPrefix2 "str2") )
          print $( lift (FamRec {famField = 'a'}) )
          print $( lift ('a' :%%: 'b') )
#endif
