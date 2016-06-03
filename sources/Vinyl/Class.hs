{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}

{-| typeclasses for @vinyl@ records, @newtype@s thereoef,
and custom representations.

-}
module Vinyl.Class where
-- import Vinyl.Class.Extra

import Data.Vinyl (Rec(..), type (∈))
import Data.Vinyl.TypeLevel

--TODO final? initial? other?
--------------------------------------------------------------------------------

{-| intial encoding: constructing heterogeneous sequences.

same kind as 'Rec':

@
record :: (k -> *) -> [k] -> *
@

same associativity/precendence as (':&'):

@
infixr 7 \`cons`
@

the two methods are like two cases in a sum type.

a 'Rec' is a concrete "initial record".

-}
class InitialRecord (record :: (k -> *) -> [k] -> *) where

  nil :: record f '[]

  cons :: f a -> record f as -> record f (a ': as)
  infixr 7 `cons`

{-|@
nil  = 'RNil'
cons = (':&')
@-}
instance InitialRecord Rec where
  nil  = RNil
  cons = (:&)

--------------------------------------------------------------------------------

{-|

the method is like a product of handlers.

-}
class FinalRecord (record :: (k -> *) -> [k] -> *) where
  record
   ::                           b
   -> (forall x. f x -> b -> b)
   -> record f as

{-|@
record nil' cons' = \case
    'RNil'      -> nil
    (f ':&' fs) -> f `cons'` fs
@-}
-- instance FinalRecord Rec where
--   record nil' cons' = \case
--       RNil      -> nil'
--       (f :& fs) -> f `cons'` fs

{-|@
record = 'R'
@-}
instance FinalRecord R where
  record = R

--------------------------------------------------------------------------------

{- | a concrete "final record" ('FinalRecord').


-}
data R f as = forall b. R --NOTE -XExistentialQuantification
 { _nil  :: b
 , _cons :: forall a. (a ∈ as) => f a -> b -> b -- TODO
 }

{-old
, _cons :: forall a. f a -> R f as -> b
, _cons :: forall a. f a -> b -> b
, _cons :: forall a. (a ∈ as) => f a -> b -> b

-}

--------------------------------------------------------------------------------

{-
rappend
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
rappend RNil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

-- matches and builds. intial encoding only builds.
-}

-- rappend
--   :: (FinalRecord record)
--   => record f as
--   -> record f bs
--   -> record f (as ++ bs)
-- rappend r1 r2 = record nil' cons'
--   where
--   nil'  = undefined
--   cons' = undefined
--   --  if the first is nil, then return the second
