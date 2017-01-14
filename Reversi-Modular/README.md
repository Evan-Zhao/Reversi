# REVERSI (Modular)

This is a WIP version of reversi so it is committed as another branch; it consists of the attempt to make all actions modular, including player moves and command-line options, so that actions can add/switch/remove other actions, in a task queue.

It turns out that to implement "global state" requires State Monad if one feels the urge to avoid boilerplate code, to pass state parameters around and around, and in combination with IO Monad this becomes a rather involved task (StateT transformer). Hence these code is archived until when Monad Transformer becomes a relatively easier thing to deal with.
