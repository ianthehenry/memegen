The following build instructions are for Mac users. It's a lot more straightforward on Linux.

This project relies on `cairo` and `pango`. If you don't have them already, you can install them with Homebrew like this:

    $ brew install cairo --without-x11
    $ brew install pango --without-x11

Afterwards, I'm pretty sure you need `gtk2hs-buildtools` for something.

    $ cabal install gtk2hs-buildtools

Make sure you don't install that into a sandbox! It adds some binaries that you need. So make sure `~/.cabal/bin` is on your `PATH`.

Afterwards, we do haskell things:

    $ cabal sandbox init

We have to install the Haskell `pango` bindings separately because `clang` can't build it. Look, don't ask me.

    $ cabal install pango -j --with-gcc=gcc-4.8

Then we install the rest:

    $ cabal install -j --only-dependencies

And we're done! ✨

    $ cabal run

If all goes according to plan:

![Truly a success we can all appreciate](screenshot.png)
