{-# LANGUAGE Safe #-}

module KLMC.Types where

import Control.Monad.Reader
import Control.Monad.Writer (WriterT)
import Data.Default

import Data.Map
import KLMC.Keys

-- * Layout representation

-- | General configuration for a layout.  Where exactly this ends up
-- is compiler-dependant.
data Config = Config
  { name :: String }
   deriving (Eq, Ord, Read, Show)

-- | KLMC treats a layout as fundamentally a state machine, with a
-- twist: there are two parallel state systems, States and Layers.
-- States are used to implement dead keys: dead keys enable a state,
-- and bindings are state-dependant.  Layers are used to implement
-- shift and other modifiers.
--
-- This makes some sense because a State machine is a more complex
-- abstraction than layers, that's better be ignored when not configuring
-- dead keys.  Most systems don't allow to express dead keys as a
-- state machine, so states will have to be reduced to the nearest
-- approximation.
data State =
  NormalState
  | DeadState
    { stateId :: String
    , combiningChar :: Maybe Char
    , repr :: String }
    deriving (Eq, Ord, Read, Show)

-- | Most modifiers are duplicated left and right, and MacOS at least
-- supports this.
data ModifierState = Off | Any | Left | Right
  deriving (Eq, Ord, Read, Show)

-- | Modifier mask for a layer.
--
-- Only MacOS fully supports this.  For Windows, this gets degraded by
-- collapsing all the non-shift modifiers into a single (commandState
-- Any or controlState Any or altState Any) and using it as the VK
-- binding.  It warns if the choice is ambiguous.
--
-- On XKB, this gets dropped entirely, except for shift and capslock.
-- The XKB generator tries to guess the correct caps mode for the
-- value of caps, and will complain if it cannot.
data ModifierMask = ModifierMask
  { capsLockState :: Bool -- Caps lock
  , commandState :: ModifierState -- command key (osx only)
  , controlState :: ModifierState -- ctrl key
  , altState :: ModifierState -- alt/option on OSX
  , shiftState :: ModifierState }
  deriving (Eq, Ord, Read, Show)

instance Default ModifierMask where
  def = ModifierMask False Off Off Off Off

-- | A Layer descriptor, as a level and a state of modifiers.  MacOS
-- doesn't have levels, though.  @FIXME How to handle level>0 on mac
-- generator? Ignore entirely, treat as anyOption?  In this case ->
-- need to detect abnormal description.
--
-- On MacOS, level == 1 gets translated to an anyOption map.  It is an
-- error in the macos generator to both have keys bound in an
-- ModifierMask with option non-Off and a level>0.
--
-- Conversely, if there's no level>0 but there are keys bound in a
-- ModifierMask where command and control are off and option non-off,
-- they'll get handled as level 1 (the altGr layer, which XKB calls
-- Level 3).

data Layer = Layer
  { level :: Int -- 0 for base, 1 for altGr, n… for further levels.
  , modifiers :: ModifierMask }
  deriving (Eq, Ord, Read, Show)

-- | A dumb generator for an infinite list of layers. ShitMod only.
simpleLayers = layers' (Layer 0 def)
  where layers' p = (p:layers' (nextLayer p))
        nextLayer (Layer l m) | shiftState m == Off = Layer l (m { shiftState = Any })
        nextLayer (Layer l m) | otherwise = Layer (l + 1) $ (m { shiftState = Off })

-- | What a key does!
data Effect = Terminal Output -- Emit Output
            | EnterState State -- Enter State
            | Combine -- In a dead state with a combining character,
                      -- combine with character in the corresponding
                      -- level.  In normal state, do nothing.
            | Unbound -- Do nothing.
  deriving (Eq, Ord, Read, Show)

-- | Two possible outputs: text or control (F-keys, arrow…)
data Output = Character Char
            | Control -- TODO
  deriving (Eq, Ord, Read, Show)

-- | Effectuable is a class to simplify notation. See instances.  Eg,
-- This lets users bind to 'A' instead of Terminal $ Character
-- 'A'.
class Effectuable a where
  asEffect :: a -> Effect


-- | The very interesting Effectuable instance for Effect.
instance Effectuable Effect where
  asEffect = id

-- | Effectuable instance for Char.
instance Effectuable Char where
  asEffect c = Terminal $ Character c

-- | Another class to simplify notation, so we can express layered
-- bindings with tuples or lists.
class Layerable a where
  asLayers :: a -> Map Layer Effect

-- At this point, everything is in place. I think the best binding
-- model is a map of maps:

type Layout = Map State (Map Key (Map Layer Effect))

-- | Keys are Ints in disguise.
newtype Key =  Key Int

-- * Compiler

-- | A Compiler turns a layout into a series of named files.
type Compiler = Config -> Layout -> [(FilePath, String)]
