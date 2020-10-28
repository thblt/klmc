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

-- ** States

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

-- | Layers are a complex thing, because they're really the most
-- platform-specific of all abstractions.
--
-- XKB's layers are called levels.  Odd levels are the base level (1)
-- and the higher levels (3, 5, 7); even levels are the shifted
-- version of the above.  LevelN keys must be bound to enter odd
-- levels>1; AltGr usually does this for level 3, further keys can be
-- bound to 5 and 7 at least.
--
-- In OSX, those are called maps, and are associated to various
-- combinations of modifiers, *including capslock*.  It is worth
-- noting that creating a map on the command modifier does *not* turn
-- Command-key into a character input combination.  Instead, it makes
-- Command emit that key combined with itself. This is useful for
-- non-latin layouts to be able to emit the standard bindings; a Greek
-- layout could for example bind x (the letter X) to the χ (the letter
-- chi) key on the command map, so Command-χ (Command-Chi) would emit
-- Command-X, cut.
--
-- In Windows, combinations of modifiers (mapped VKs) enter a
-- "shiftState", and a modifier table determine which shiftstate are
-- valid for entering characters. Unlike what MacOS does, this doesn't
-- modify bindings: if Ctrl-Alt-χ is bound to X, and χ is on VK_A,
-- then pressing Ctrl-Alt-χ will either emit the character X, *OR*, if
-- the application has registered it, the shortcut Ctrl-Alt-χ, *OR*,
-- if the application is listening to VK_A, Ctrl-Alt-VK_A that gets
-- translated to Ctrl-Alt-A.
--
-- A possible limitation of Windows is that the level-up key (ALtGr)
-- is always emulated and translated to Ctrl-Alt, which may conflict
-- with app bindings.
--
-- Because of this variety, Compilers will probably expose a
-- translation function from Layer (or more exactly, [a] defaulting to
-- [Layer]) to their preferred internal representation.
data Layer = Layer
  { level :: Int -- 0 for base, 1 for altGr, n… for further levels.
  , modifiers :: ModifierMask }
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

-- | Most modifiers are duplicated left and right, and MacOS at least
-- supports the distinction.  Windows too, I think.
data ModifierState = Off | Any | Left | Right
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
