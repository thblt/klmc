module KLMC.Compilers.XKB where

import KLMC.Types

data XKBConfig = XKBConfig
  { extraIncludes :: [String] -- | Extra includes to append after the mapping definition.
  , composeFilename :: String }

-- | Run the two auxiliary compilers in sequence: `xkbCompiler`,
-- | then `composeCompiler`.
compiler :: Compiler
compiler = undefined

-- | Generate the XKB file.
xkbCompiler :: XKBConfig -> Compiler
xkbCompiler = undefined

-- | Generate the Compose file.
composeCompiler :: XKBConfig -> Compiler
composeCompiler = undefined
