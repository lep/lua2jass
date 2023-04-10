module Data.MonoidMap where

import Data.Map (Map)
import qualified Data.Map as Map

newtype MonoidMap k v = MonoidMap { getMonoidMap :: Map k v }
    deriving (Show)

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
    (MonoidMap a) <> (MonoidMap b) = MonoidMap $ Map.unionWith (<>) a b

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
    mempty = MonoidMap mempty

singleton :: (Ord k, Monoid v) => k -> v -> MonoidMap k v
singleton k v = MonoidMap $ Map.singleton k v

lookup :: (Ord k, Monoid v) => k -> MonoidMap k v -> Maybe v
lookup k (MonoidMap m) = Map.lookup k m

lookup' :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
lookup' k (MonoidMap m) = Map.findWithDefault mempty k m

