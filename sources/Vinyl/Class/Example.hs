{-# LANGUAGE DataKinds, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Vinyl.Class.Example where
import Vinyl.Class

import Data.Vinyl
import Data.Vinyl.Functor

--------------------------------------------------------------------------------

{-|
@
stack build && stack exec -- vinyl-class-example
@
-}
main :: IO ()
main = do
 putStrLn ""
 print $ rec_vinyl

--------------------------------------------------------------------------------

{-| Inferred:

@
rec :: (Record record, Num i) => record Identity [Bool,i,String]
@

-}
rec = Identity False `cons` Identity 0 `cons` Identity "" `cons` nil

rec_vinyl :: Rec Identity [Bool,Int,String]
rec_vinyl = rec
