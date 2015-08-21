# Hello, World!

[![Build Status](https://travis-ci.org/wildlyinaccurate/hello-world.svg?branch=master)](https://travis-ci.org/wildlyinaccurate/hello-world)

Hello, World! Is a programming language for writing hello world applications. And nothing else.

## Installation

```
git clone https://github.com/wildlyinaccurate/hello-world.git
cd hello-world
caban sandbox init
cabal install
make
```

## Usage

Hello, World! can be run as a REPL

```
$ ./bin/hello-world
hello=>   Hello, World!
Hello, World!
hello=> Hello, World!
Parse error at "lisp" (line 1, column 1):
unexpected "H"
expecting "  Hello, World!"
hello=> what even is this
Parse error at "lisp" (line 1, column 1):
unexpected "w"
expecting "  Hello, World!"
```

Or it can take programs directly

```
$ ./bin/hello-world '  Hello, World!'
Hello, World!
```

## Why?

Shut your face.
