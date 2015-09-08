# Hello, World!

[![Build Status](https://travis-ci.org/wildlyinaccurate/hello-world.svg?branch=master)](https://travis-ci.org/wildlyinaccurate/hello-world)

Hello, World! Is a programming language for writing hello world applications. And nothing else.

## Syntax

### Hello, World!

The <code>&nbsp;&nbsp;Hello, World!</code> token (two spaces, followed by the string "Hello, World!") will evaluate to the string `"Hello, World!"`. This token is represented internally as a `HelloVal`. Once the interpreter parses a `HelloVal`, it will not parse anything else.

### Comments

Since everything after a `HelloVal` is ignored, it is possible to document your programs with comments like so:

```
  Hello, World! This is a comment
  Hello, World! This is another, slightly more interesting comment
  Hello, World!
```

The program above will output the following:

```
Hello, World!
Hello, World!
Hello, World!
```

### Boolean Logic

You can implement simple boolean logic by utilising the fact that anything other than a `HelloVal` is an error.


```
  Hello, World!
False
  Hello, World! This will never be evaluated
```

The above program will conveniently explode on the `False` statement, and output the following:

```
Hello, World!
Parse error at "your face" (line 1, column 1):
unexpected "F"
```

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
