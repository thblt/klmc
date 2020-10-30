-- | Utilities for compilers.

module KLMC.Compilers.Util where

import Data.Map as M
import Data.Set as S
import KLMC.Types

-- | Translate a layout to a compiler's preferred representation of
-- states, keys, layers and effects.  All the translators are
-- independant.  It's sometimes useful to prepare the layer translator
-- with `layers`.
translateLayout :: (s -> s') -> (k -> k') -> (l -> l') -> (e -> e') -> (Layout s' k' l' e')
translateLayout = undefined

filterLayout :: Layout s k l e -> (s -> k -> l -> e -> Bool) -> Layout s k l e
filterLayout = undefined

flattenLayout :: Layout s k l e -> [(s, Map k (M.Map l e))]
flattenLayout = M.toList


-- flattenLayout' :: Layout s k l e -> Map s (Map k [(l, e)])
-- flattenLayout' :: Map a (Map b (Map c d) ) -> []
flattenLayout' x =  M.toList (fmap M.toList $ (fmap . fmap) M.toList x)


-- | Collect all unique layers in a layout.
layers :: Layout _1 _2 l _3 -> Set l
layers l = undefined

m :: Map String String
m = M.fromList
  [ ("hello", "33")
  , ("i", "34")
  , ("live", "12")
  , ("you", "21")]
