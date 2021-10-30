module KLMC.Examples.Bépo where

import KLMC
import KLMC.Keys
import KLMC.Types

bépo = do
  withLayers [ layer 0 def
             , layer 0 shifted
             , layer 1 def
             , layer 1 shifted ]
  --                  {- Base -} , {- S/hift + caps -} , {- Level3/AltG-} , {- 3+shift -}
  map kQ           ( 'b'        , 'B'                , '|'              , '_'         )
  map kW           ( 'é'        , 'É'                , deadAcute        , '♥'         )
  map kE           ( 'p'        , 'P'                , '&'              , '§'         )
  map kR           ( 'o'        , 'O'                , 'œ'              , 'Œ'         )
  on deadAcute     ( 'ó'        , 'Ó' )
  map kT            ( 'è'        , 'È'                , deadGrave        , '`'         )


bepoConfig = Config
  { name = "BÉPO (French, ergonomic, like Dvorak, NF Z71-300)" }

vkLayer = Layer

virtualAzerty = do
  setLayers allControlStateLayers 0 -- Bind on ctrl-*, alt-*, cmd-…*
  map kQ 'a'
  map kW 'z'
  map kE 'e'
  map kR 'r'
  map kT 't'
  map kY 'y'

deadAcute = DeadState "dead_acute" (Just '\x0301') "´"
deadGrave = DeadState "dead_grave" (Just '\x0300') "`"

state = undefined
bind = undefined
(<->) = undefined
data Layout
