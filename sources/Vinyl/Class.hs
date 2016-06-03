{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}

{-| typeclasses for @vinyl@ records, @newtype@s thereoef,
and custom representations.

-}
module Vinyl.Class where
-- import Vinyl.Class.Extra

import Data.Vinyl

--TODO final? initial? other?
--------------------------------------------------------------------------------

{-| intial encoding: constructing heterogeneous sequences.

same kind as 'Rec':

@
record :: (k -> *) -> [k] -> *
@

same associativity/precendence as (':&'):

@
infixr 7 `cons`
@

-}
class Record (record :: (k -> *) -> [k] -> *) where

  nil :: record f '[]

  cons :: f a -> record f as -> record f (a ': as)
  infixr 7 `cons`

{-|@
nil = 'RNil'
cons = (':&'')
@-}
instance Record Rec where
  nil = RNil
  cons = (:&)

--------------------------------------------------------------------------------
