klmc
====

`klmc` is a compiler for keyboard layouts.  Layouts are Haskell
programs that compile to XKB layout and Compose file (GNU Linux/BSD),
Microsoft Windows C source and MacOS' `keylayout` files.  As these are
probably the hardest formats to implement, other targets should be
easy to add.

Layout format
-------------

Internally, a layout is a

``` haskell
newtype Layout = Map State (Map Key (Map Layer Effect))
```

That is, a map of `State` of maps of keys to maps associating `Layer`s
to `Effect`s.  Internally, the layout is a sort of state machine,
where states are activated by dead keys.  Let's go through this right
to left.

 - An `Effect` is what a key *does*.  There are three main effects:
   emit a character, like "K", "a", "1" and so on; emit a control
   character, like an arrow, the backspace key, or return, or enter a
   state, that is, modify the mapping of further key presses.  States
   are essentially used to describe dead keys.

 - A `Layer` is an abstraction of the various shifted, capslocked,
   variants of a layout.  Layers are described as both modifier states
   and a number.  Usually, the principal use of layers is to describe
   shift/altgr variants of a layout, but in `klmc` layers (for targets
   that support it) can be defined for any combination of modifiers.

 - A `State` is activated by a dead key and modifies the full layout
   accordingly, on all layers.  For example, the dead acute accent key
   will enter a state where the letter E emits "é" in base layer, "É"
   in shitfed/caps layer, and so on.

   States may feel like an unusual approach to dead keys.  Indeed,
   most OS don't actually treat dead keys as layout transformers, but
   as character combinations (eg, XKB uses a character map for its
   dead keys, not a key map).  The reason we use states is that they
   match the very powerful representation macOS uses, so we can expose
   the full power of macOS keylayout format, with a simple degradation
   mechanism for more limited targets.

   Notice, though, that you actually don't need to be explicit about
   the obvious combinations, like dead accents: klmc is smart enough
   to combine a dead acute accent and a letter for you.  States are
   only really useful for more complex dead keys, like a "dead Greek"
   key that would combine like `dead_greek + a = α`.

Limitations of exporters and degradation strategies
---------------------------------------------------

Not every target platform supports the full power of the klmc layout
representation.  This is something you should take into consideration
when designing layouts.  The following table summarizes these features
and each target's limitations in their regard.

 1. **States**
   - **XKB** doesn't support states, which are thus degraded to
     character combinations. This makes it impossible to have the same
     character on two keys at base state, but make them emit different
     characters on a dead key state.
   - **MacOS** fully supports states.
   - **Windows** support probably the same as XKB.
 2. **Unlimited layers**
   - **XKB** has up to eight levels, including shifted levels, maybe
     more, I need to check.  That means three AltGr-ish keys.
   - **MacOS** supports as many layers as can be expressed with its
     standard modifiers.
   - **Windows** TBD.
 3. **Layers on special modifiers**
   - **XKB** doesn't support that, and layers that use Ctrl and other
     control modifiers are dropped at compilation.  Or [maybe it
     does](https://unix.stackexchange.com/questions/609605/xkb-transparently-map-modifier-level/610330#610330).
   - **MacOS** fully supports it.  If you have a key that's "é" in the
     base layer, you can bind it to "c" in the "command key pressed"
     layer.  In this case, Command-é actually emits Command-C, the
     copy shortcut (but *not* the letter C).
   - **Windows** has an alternative mechanism called *Virtual keys*.
     Applications actually bind to virtual keys, not actual letters.
     In the above example, the same key would be bound to é and `vk_c`
     (the virtual C).  Klmc handles this by collapsing all the
     modifier layers into one, and converting it into Windows virtual keys.
