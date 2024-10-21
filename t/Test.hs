{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Foo
import Language.Haskell.TH.Syntax

main :: IO ()
main = do print $( lift (Foo "str1" 'c') )
          print $( lift (Bar "str2") )
          print $( lift (Rec {field = 'a'}) )
          print $( lift (Unboxed 'a'# 1.0## 1.0# 1# 1##) )
          print $( lift (In { out = Nothing }) )
          print $( lift (FamPrefix1 "str1" 'c') )
          print $( lift (FamPrefix2 "str2") )
          print $( lift (FamRec {famField = 'a'}) )
          print $( lift ('a' :%%: 'b') )
