
# Calculus Typed #

In this repository you'll find the `calc2` software used to build
the "Calculus Typed" textbook. You can check out the textbook at
[https://calculustyped.taylorhummon.com/](https://calculustyped.taylorhummon.com/).
The book's source files are available in a separate companion repository, `calculus_typed_book`.

The book's source is stored in files with the extension `.calc`. These files
are written in a simple markup language designed specifically for the particular
mathematics typesetting needs of this project. The `calc2` software can be run in
one of two modes:
* `calc2web` converts the `.calc` files to a web representation of
the textbook (`.html`, `.js`, and `.css`), and
* `calc2tex` converts the `.calc` files to a `TeX` textbook (and hence `.pdf`).


## Setup ##

To build this software, you'll need to install the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) at version 8.0 or higher. I recommend using [GHCup](https://www.haskell.org/ghcup/) to install the compiler and several commonly used Haskell tools.

You'll also need [Mathjax](https://www.mathjax.org/) 2.7.7. We use this package
for pretty math output on the web.


## Project Layout ##

Here's the directory layout and some of the more important files.

    book                      # a symbolic link to the book's source
    calc2/                    # contains the code that builds the book
    calc2/calc2               # the application that builds the book
    resources/                # web and tex assets
    tex/                      # tex output
    web/                      # web output for developing locally
    publish/                  # web output for deployment


## Compiling calc2 ##

Compile the `calc2` application using the following command:

    ghc calc2

You can then run

    ./calc2

for a quick description of usage.


## Building Web Output ##

For incremental changes, we can build output using:

    ./calc2web

But if there is a major structural change, instead use:

    ./calc2web-rebuild

This will delete the web directory and build everything back.

To build for uploading, use:

    ./calc2web-publish

This builds the site in a way that is ready for publishing, placing
the results in `publish/`.


## Compiling TeX to PDF ##

After running,

    ./calc2tex

we run,

    pdflatex calculustyped

from the `tex/` directory.

When `calculustyped.pdf` is good to go online, copy it to the
resources directory and run `calc2web-publish`.


## Mathjax ##

We use Mathjax for mathematics on the web. Locally, we symlink in the
Mathjax library from:

    /usr/local/share/javascript/mathjax/

On the server we keep a copy of this library at

    ~/ct/auto/publish/mathjax/

but it is not included in the Git repository.
