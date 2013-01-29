Bilder is a Domain Specific Language (DSL) for Image Processing, developed as a
Master's Thesis project by two students at Chalmers University of Technology.
The compiler takes image filter files written in the Bilder language and spits
out GLSL fragment shaders along with a JSON graph to represent the order of
shader execution. See our [report](TODO) for more information on how stuff
works.

## Live demo
An on-line version of the compiler is available at
[http://bilder.htn.se](http://bilder.htn.se). Please send us a message if
something isn't working with it!

## Building the Compiler
To build the Bilder Compiler (Bilder Builder), a more recent version of the GHC
compiler is required (the build succeeds with an older version, but the results
aren't quite right).

The following dependencies has to be met to build the compiler:

* ghc >= 7.6.1
* bnfc >= 2.5b
* happy (tested with 1.18.10)
* alex (tested with 3.0.2)
* cabal (tested with 1.16.0.2)

These (except ghc) and some other stuff needed are
available in the Cabal:

    cabal update
    cabal install bnfc happy alex json hashable regex-posix regex-compat cgi

To compile, just run make:

    make

### Cabal

If you don't have cabal-install installed, you can get it either from your
package manager, or at
[http://hackage.haskell.org/package/cabal-install](http://hackage.haskell.org/package/cabal-install).
To install from the .tar.gz, run

    sh bootstrap.hs
    cabal update
    cabal install cabal-install

## Precompiled binaries
Freshly baked binaries can be found for 32-bit and 64-bit linux architectures.

* [linux-x86](http://bilder.htn.se/builds/linux-x86/)
* [linux-x86_64](http://bilder.htn.se/builds/linux-x86_64/)

## Using the Bilder Builder
Binaries produced by make are placed under `bin/`, and includes the command line
version `bildc` and it's cgi counterpart `cgibildc` (see [Bilder
Demo](https://github.com/ingemaradahl/bilder-demo) for more information for what
to do with the cgi compiler).

To compile a filter program (a bild), give it to the compiler as such:

    bildc cool_image_filter.bild

## Compiler limitations
As stated in the report of our thesis, the compiler does not fully implement the
Bilder language, but acts as a prototype implementation supporting the essential
features of image filter computation and composition. For instance, higher order
functions are not fully supported, but only functions taking other functions of
an image-type. Also, since the compiler targets GLSL, some of the limitations
present in GLSL will propagate to the produced filters, such as the restriction
of recursive functions.

## Licence
The Bilder Compiler is licensed under the GNU Lesser General Public License, see
LICENSE for more information.
