## Heist

Heist is a Haskell templating library commonly used with the Snap Framework.
Heist keeps business logic out of the templates by giving you facilities for
manipulating the templates in code. For instance, extracting and repeating
template nodes from code instead of having a special template syntax for looping
and conditions.

## Digestive Functors

Digestive Functors is a Haskell form library inspired by Formlets and providing
a succinct API for building and validating forms. You can use this library
independent of any web framework or templating system, but the examples here
will use it exclusively with Snap and compiled Heist.

## digestive-heist-demos

Although each of the libraries used here have nice documentation and a few
examples, there aren't many (or at the time of this writing, *any*) examples
showing how they all work together. The aim of this site is to show off how
easy and effective it is to build a form-heavy website using this libraries,
and to offer a fully working codebase that you can clone and experiment with.

## Build steps (using cabal sandboxes)

This was last built with ghc 7.6.3 and cabal 1.18.0. These steps assume you have
those installed at the same major versions (ghc in particular sometimes
introduces breaking changes in new major versions, so no guarantees for this
building easily in future versions).

1. git clone git@github.com:ericrasmussen/digestive-heist-demos.git
2. cd digestive-heist-demos
3. cabal sandbox init
4. cabal install --only-dependencies
5. (wait patiently)
6. cabal build
7. dist/build/snap-heist-examples/snap-heist-examples -p 3000

The last step will launch the app and server locally on port 3000 (you can use a
different port if you like, and it will default to 8000 if unspecified).

At that point you can interact with it in your browser (usually localhost:3000),
modify the templates and restart the application to see the changes, or change
the code and run cabal build again.

Have fun!
