# What's the big idea

It's a bot for Slack that generates memes! But don't take my word for it. *Let me show you*.

> /meme success-kid built a meme generator | in haskell

![A success we can all appreciate](screenshot.png)

# How does it work

It works because of [tightrope](https://github.com/ianthehenry/tightrope), a magical library that makes it easy to write slack bots. And also because of Cairo (a rendering library) and Pango (a text layout library).

# I wanna run it myself

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

Okay! Now we can use it. But first we need to edit the `conf` file and fill in useful values -- like a token so it can post memes back to Slack, and the domain of the slack account you want to run it on.

    $ vim conf

And that's it!

    $ cabal run
