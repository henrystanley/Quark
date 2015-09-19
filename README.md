Quark
=====

Quark is a microscopic stack based programming language with a focus on minimalism and flexibility.
It is intended primarily as an exercise in simplicity and language design.
Quark's core principle is to bootstrap as much of the language as possible.
Thus the entire language is currently described by a mere, 22 functions.

To install you'll need GHC and cabal.
Assuming you have them, installation should be as easy as:

    $ cabal install

I make no promises on the likelihood of this succeeding, however.

Quark can be invoked with a filename to run a quark script (`.qrk`),
or without to start the repl:

    $ quark hallowelt.qrk
    Hallo Welt!
    $ quark
    :>

For an intro to the language check out 'intro.md'.
For the language documentation give 'api.yaml' a go.
