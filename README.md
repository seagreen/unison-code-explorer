# About

Code explorer for the [Unison](https://www.unisonweb.org/) language. Inspired by [haskell-code-explorer](https://github.com/alexwl/haskell-code-explorer).

See it live [here](http://unison.readvar.com/).

# Status

Heavily WIP.

In addition to lots of minor issues there are three serious enough to mention here:

+ Back button support / URL entrypoints into app ([issue #5](https://github.com/seagreen/unison-code-explorer/issues/5))

+ Notify the user when the websocket disconnects ([issue #4](https://github.com/seagreen/unison-code-explorer/issues/4))

+ Auto-reconnect for a certain amount of time ([issue #6](https://github.com/seagreen/unison-code-explorer/issues/6))

# Technology

*Unison Code Explorer* is built on [concur-replica](https://github.com/pkamenarsky/concur-replica).

# Use

1. `stack install`

2. `cd` to a directory containing a `.unison` codebase.

3. `unison-code-explorer`

4. Visit `localhost:8080` in your web browser.
