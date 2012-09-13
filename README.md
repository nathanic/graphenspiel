# Graphenspiel

Graphenspiel is a graph-based musical toy.

It is very incomplete at this point.

I'm using [Quil](https://github.com/quil/quil) (and thereby [Processing](http://processing.org)) for the UI.

Eventually it will likely use [Overtone](http://overtone.github.com/) to produce sounds, although I might offer an easier-to-setup fallback mode to a less comprehensive JVM synthesizer lib.

I have also thought about making it generate generic [OSC](http://en.wikipedia.org/wiki/Open_Sound_Control) events that could be used in any number of audio systems.

## Usage

Right now the only way to run it is to interact from a REPL or something like SLIME/VimClojure.  I personally use SLIMV.

In the future `lein run` will do something useful.


## License

Graphenspiel is licensed under the WTFPL, the most permissive possible license.
Full terms of the license are available at [wtfpl.org](http://wtfpl.org/).

Copyright © 2012 Nathan Stien

