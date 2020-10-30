{-|
Module      : KLMC.Designer
Description : A design DSL for layouts
Copyright   : © 2020 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental

Designer is a DSL for keyboard layouts, that tries to be concise,
expressive and useful.
-}

module KLMC.Designer where

import Control.Monad.Identity (Identity(Identity))
import Control.Monad.State as MS
import Control.Monad.Writer as MW
import Data.Default
import Data.Maybe (maybe)
import KLMC.Types

-- | A Designer is a Writer in a State.

type Designer s k l e = StateT (DesignerState s k l e) (WriterT [(s, k, l, e)] Identity)

data DesignerState s k l e = DesignerState
  { desLayers :: [l]         -- | The set of Layers in this layout.
  , desState :: Maybe s      -- | The current state
  , desKey :: Maybe k        -- | The current key
  , desLayer :: Maybe l      -- | The current layer
  , desEffect :: Maybe e }   -- | The current effect

type LayoutData s k l e = [(s, k, l, e)]

instance Default (DesignerState s k l e) where
  def = DesignerState [] Nothing Nothing Nothing Nothing

onState :: String -> (Designer String k l e ())
onState s = do
  st <- MS.get
  MS.put $ st { desState = Just s }
    return ()

onKey :: String -> (Designer s String l e ())
onKey k = do
  st <- MS.get
  MS.put $ st { desKey = Just k}
  return ()

-- | Map key k to layers l.
map :: Layerable b => k -> b -> Designer s k Layer e ()
map k b = (asLayers b)

test :: (Designer String String l e String)
test = do
  onState "Hello"
  onState "Yeah"
  onKey "Aleph"
  getState

bind :: (Designer String k l e ())
bind = do
  a <- onState "Pouet"
  b <- onState "Canard"
  return ()

-- getState :: Designer String k l e String
--   getState = do
--   st <- MS.get
--   return $ maybe "" id $ DesignerState st
