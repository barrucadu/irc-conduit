# [irc-conduit]: IRC message encoding and decoding + networking

 - Provides [conduits][conduit] for translating bytestrings into
   "events", and "messages" into bytestrings.

 - Provides a sum type for all IRC messages you're likely to want to
   deal with in a client.

 - Provides two helper functions for connecting to IRC servers
   directly.

 - Manages flood protection when connecting to a server directly.

Note: this used to be a part of [yukibot][], so if you want the history
from before this was split out into its own library, check there.

[irc-conduit]: http://hackage.haskell.org/package/irc-conduit
[conduit]:     https://hackage.haskell.org/package/conduit
[yukibot]:     https://github.com/barrucadu/yukibot
