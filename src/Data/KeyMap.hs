{-# LANGUAGE
        FlexibleInstances,
        FunctionalDependencies,
        MultiParamTypeClasses
  #-}
-- This module uses multi-parameter type-classes with functional dependencies.
-- Associated types would allow for a more readable implementation.

module Data.KeyMap (

  KeyMap, empty, null, lookup, alter, combine, toList,

  insert, adjust, delete,

  unionWith, union, symDiff,

  updateWith, difference, update,

  intersectionWith, intersection,

  map, mapMaybeWithKey, 

  fromList

  ) where

import Prelude hiding ( null, lookup, map )
import qualified Data.Map as M
import qualified Data.IntMap as IM

just :: a -> Maybe a
just = (Just $!)

-- The dependency "key -> map" is not necessary albeit convenient.
class KeyMap key map | map -> key where
  empty   :: map val
  null    :: map val -> Bool
  lookup  :: key -> map val -> Maybe val
  alter   :: key -> (Maybe val -> Maybe val) -> map val -> map val
  combine :: (Maybe val -> Maybe val' -> Maybe val'')
          -> map val -> map val' -> map val''
  mapMaybeWithKey :: (key -> val -> Maybe val') -> map val -> map val'
  --combine = error "combine not implemented"
  toList :: map val -> [val]
  toList = error "toList not implemented"

insert :: KeyMap key map => key -> val -> map val -> map val
insert key = alter key . const . just

adjust :: KeyMap key map => key -> (val -> val) -> map val -> map val
adjust key alt = alter key (>>=just.alt)

delete :: KeyMap key map => key -> map val -> map val
delete key = alter key (const Nothing)

unionWith :: KeyMap key map
          => (val -> val -> Maybe val) -> map val -> map val -> map val
unionWith f
  = combine (\mx my -> maybe my (\x -> maybe mx (\y -> f x y) my) mx)

union :: KeyMap key map => map val -> map val -> map val
union = unionWith (\x _ -> just x)

symDiff :: KeyMap key map => map val -> map val -> map val
symDiff = unionWith (\_ _ -> Nothing)

updateWith :: KeyMap key map
           => (val -> val' -> Maybe val) -> map val -> map val' -> map val
updateWith f
  = combine (\mx my -> mx >>= \x -> maybe mx (\y -> f x y) my)

difference :: KeyMap key map => map val -> map val' -> map val
difference = updateWith (\_ _ -> Nothing)

update :: KeyMap key map => map val -> map val -> map val
update = updateWith (\_ y -> just y)

intersectionWith :: KeyMap key map
                 => (val -> val' -> Maybe val'')
                 -> map val -> map val' -> map val''
intersectionWith f = combine (\mx my -> mx >>= \x -> my >>= \y -> f x y)

intersection :: KeyMap key map => map val -> map val -> map val
intersection = intersectionWith (\x _ -> just x)

map :: KeyMap key map => (val -> val') -> map val -> map val'
map f = mapMaybeWithKey (\ _ -> Just . f)

fromList :: KeyMap key map => [(key,val)] -> map val
fromList = foldr (uncurry insert) empty


instance Ord key => KeyMap key (M.Map key) where
  empty  = M.empty
  null   = M.null
  lookup = M.lookup
  alter  = flip M.alter -- Data.Map.alter not supported in ghc 6.4 ??

--   alter key alt m
--     = maybe (maybe m (flip (M.insert key) m) (alt Nothing))
--             (\_ -> M.update (alt.Just) key m)
--             (M.lookup key m)

  combine cmb m1 m2
    = M.fromAscList $ cmbAscLists cmb (M.toAscList m1) (M.toAscList m2)

  mapMaybeWithKey = M.mapMaybeWithKey

  toList = M.elems


cmbAscLists :: Ord key
            => (Maybe val -> Maybe val' -> Maybe val'')
            -> [(key,val)] -> [(key,val')] -> [(key,val'')]
cmbAscLists _   [] [] = []
cmbAscLists cmb [] ((k,v):kvs)
  = maybe (cmbAscLists cmb [] kvs)
          (\w -> (k,w) : cmbAscLists cmb [] kvs)
          (cmb Nothing (Just v))
cmbAscLists cmb kvs@(_:_) [] = cmbAscLists (flip cmb) [] kvs
cmbAscLists cmb kv1@((k1,v1):kvs1) kv2@((k2,v2):kvs2)
  | k1 < k2
    = maybe (cmbAscLists cmb kvs1 kv2)
            (\v -> (k1,v) : cmbAscLists cmb kvs1 kv2)
            (cmb (Just v1) Nothing)
  | k1 > k2
    = maybe (cmbAscLists cmb kv1 kvs2)
            (\v -> (k2,v) : cmbAscLists cmb kv1 kvs2)
            (cmb Nothing (Just v2))
  | otherwise -- k1 == k2
    = maybe (cmbAscLists cmb kvs1 kvs2)
            (\v -> (k1,v) : cmbAscLists cmb kvs1 kvs2)
            (cmb (Just v1) (Just v2))



instance KeyMap Int IM.IntMap where
  empty  = IM.empty
  null   = IM.null
  lookup = IM.lookup
  alter  = flip IM.alter -- Data.Map.alter not supported in ghc 6.4 ??

--   alter key alt m
--     = maybe (maybe m (flip (M.insert key) m) (alt Nothing))
--             (\_ -> M.update (alt.Just) key m)
--             (M.lookup key m)

  mapMaybeWithKey = IM.mapMaybeWithKey

  combine cmb m1 m2 =
      IM.fromAscList $ cmbAscLists cmb (IM.toAscList m1) (IM.toAscList m2)

  toList = IM.elems
