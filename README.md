<img src="https://s3.amazonaws.com/github/downloads/nathanic/graphenspiel/early-screenshot.png" alt="Early Screenshot" title="Early Screenshot" align="right" />
# Graphenspiel

Graphenspiel is a graph-based musical toy.  The basic idea is that pulses travel from source nodes to sink nodes, and make noises when they arrive.

It is very incomplete at this point.  The core of the simulation is in place, but the graphics are very basic and sound is not wired up yet.

I'm using [Quil](https://github.com/quil/quil) (and thereby [Processing](http://processing.org)) for the UI.

Eventually it will likely use [Overtone](http://overtone.github.com/) to produce sounds, although I might offer an easier-to-setup fallback mode to a less comprehensive JVM synthesizer lib.

I have also thought about making it generate generic [OSC](http://en.wikipedia.org/wiki/Open_Sound_Control) events that could be used in any number of audio systems.

## Usage


Right now the main way I run it is to interact from a REPL or something like SLIME/VimClojure.  I personally use SLIMV.

You can also `lein run` or `java -jar` the [uberjar](https://github.com/downloads/nathanic/graphenspiel/graphenspiel-0.1.0-SNAPSHOT-standalone.jar) to play with it.

In the applet, you can create sink nodes with left click, and source nodes with right click.  Pressing `r` will reset to the initial state, and `q` will kill the JVM.

## License

Graphenspiel is licensed under the WTFPL, the most permissive possible license.
Full terms of the license are available at [wtfpl.org](http://wtfpl.org/).

Copyright © 2012 Nathan Stien

