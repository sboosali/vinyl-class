{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces, RecordWildCards #-}

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

@
record f as
@

TODO the functor @f@ is a "parameter", the types @as@ are the "index".

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

TODO "type index" versus "type param": the param is constant, indices vary?

-}
class FinalRecord (record :: (k -> *) -> [k] -> *) where
  record
   :: forall (f :: k -> *) (as :: [k]) (output :: [k] -> *). ()
   => (                                 output '[]      )
   -> (forall x xs. f x -> output xs -> output (x ': xs))
   -> record f as

{-old
:: forall (f :: k -> *) (as :: [k]) (output :: [k] -> *). ()
-> (record f as -> output as)
-}

-- {-|@
-- record nil' cons' = \case
--     'RNil'      -> nil
--     (f ':&' fs) -> f `cons'` fs
-- @-}
-- instance FinalRecord Rec where
--   record nil' cons' = \case
--       RNil      -> nil'
--       (f :& fs) -> f `cons'` fs

{-|@
record = 'R'
@-}
instance FinalRecord R where
 record = R

{-old
record nil' cons' = R nil' cons'
-}

--------------------------------------------------------------------------------

{- | a concrete "final record" ('FinalRecord').

e.g. mapping

@
@

e.g. folding

@
(All Num as) => R f as -> Const Int as
@

-}
data R f as = forall (output :: [*] -> *). R --NOTE -XExistentialQuantification
 { _nil  :: output '[]
 , _cons :: (forall x xs. f x -> output xs -> output (x ': xs))
 }

-- how does an R "hold" {f a}'s, like a Rec does? or does it?

{-old

{ _nil  :: b
, _cons :: forall a. (a ∈ as) => f a -> b -> b -- TODO
}

, _cons :: forall a. f a -> R f as -> b
, _cons :: forall a. f a -> b -> b
, _cons :: forall a. (a ∈ as) => f a -> b -> b

-}

--------------------------------------------------------------------------------

r2rec :: R f as -> Rec f as
r2rec R{..} = undefined

{-old
\case
 RNil -> _nil
 (f :& fs) -> _cons f fs

record RNil (:&)
-}

rec2r :: Rec f as -> R f as
rec2r = \case
  RNil -> undefined
  (f :& fs) -> undefined

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

--------------------------------------------------------------------------------

{-notes

(a + b) -> c  ~  (a -> c) * (b -> c)

Rec f as -> output as  ~  R f as

-}
