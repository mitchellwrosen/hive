#### Overview

A Haskell implementation of Hive. The game logic is interpreted in `src/Hive.hs`, and the player action monad is defined in `src/Hive/Monad.hs`.

#### Building

`stack build`

#### Implementations

- `hive-local`
  - A command-line based example implementation. Both players input moves on the same terminal.
  - To run: `stack exec hive-local`
- `hive-ws-server`
  - An example WebSocket server
  - To run: `stack exec hive-ws-server`
- `hive-ws-client`
  - An example terminal WebSocket client
  - To run: `stack exec hive-ws-client``
