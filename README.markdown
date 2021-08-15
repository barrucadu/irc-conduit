**This project is essentially abandonware!**

I may respond to minor issues, like version bounds which need
changing, but I won't be doing any significant work.

Offer to take over the package if you want any significant changes.

[irc-conduit][]
============

Streaming IRC message library using conduits.

 - Provides [conduits][conduit] for translating bytestrings into
   "events", and "messages" into bytestrings.

 - Provides a sum type for all IRC messages you're likely to want to
   deal with in a client.

 - Provides two helper functions for connecting to IRC servers
   directly.

 - Manages flood protection when connecting to a server directly.

Note
----

This used to be a part of [yukibot][], so if you want the history from
before this was split out into its own library, check there.

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on
libera.chat), or email (mike@barrucadu.co.uk).

[irc-conduit]: http://hackage.haskell.org/package/irc-conduit
[conduit]:     https://hackage.haskell.org/package/conduit
[yukibot]:     https://github.com/barrucadu/yukibot
