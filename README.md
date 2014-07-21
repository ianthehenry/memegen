To install, Mac edition:

    $ brew install cairo --without-x11
    $ brew install pango --without-x11
    $ cabal install gtk2hs-buildtools
    $ cabal sandbox init # we deliberately do this after installing gtk2hs-buildtools, because it needs to put stuff on our PATH
    $ cabal install cairo
    $ cabal install pango --with-gcc=gcc-4.8 # clang doesn't work. look don't ask me
    $ cabal run # ✨ yay ✨

