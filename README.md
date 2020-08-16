# nim-iterator-stream-experiment

[![Build Status](https://github.com/thenjip/nim-iterator-stream-experiment/workflows/Unit%20tests/badge.svg?branch=master)](https://github.com/thenjip/nim-iterator-stream-experiment/actions?query=workflow%3A"Unit+tests"+branch%3A"master")
[![licence](https://img.shields.io/github/license/thenjip/nim-iterator-stream-experiment.svg)](https://raw.githubusercontent.com/thenjip/nim-iterator-stream-experiment/master/LICENSE)

An attempt at providing a replacement for [first class iterators](
https://nim-lang.org/docs/manual.html#iterators-and-the-for-statement-first-class-iterators)
in [Nim](https://nim-lang.org/) with an API similar to Java 8 Stream.

## Installation

```sh
nimble install 'https://github.com/thenjip/nim-iterator-stream-experiment'
```

### Dependencies

`nim` >= `1.2.0`

## Documentation

[Read the docs](https://thenjip.github.io/nim-iterator-stream-experiment).

## Rationale

First class iterators are not available in Nim's VM and JavaScript back ends.
This project exists to see if implementing an iterator API compatible with all
Nim's back ends is possible (including compile time execution).

First class iterators are the priority, but it turns out any iterator that does
not rely on compiler magic can be replaced (example: the [iterator](
https://nim-lang.org/docs/iterators.html#fields.i%2CT) on structure fields).

The API is inspired by Java 8 Stream but the implementation is almost entirely
based on functional programming.

Many types used to implement this API rely on closure procedures. So they can
not be assigned to `const` symbols, but they can still be part of computations
at compile time.

## Back end compatibility

The results below are based on the status of CI builds.

| Back end | @ Run time | @ Compile time |
| :--- | :--- | :--- |
| C | Supported | Supported |
| C++ | Supported | Supported |
| JavaScript | Not ready yet. The unit tests do not compile. | Same as "@ run time" |
| NimScript | To be tested | To be tested |

## Usage

### Processing characters from a string

```Nim
import nim_iterator_stream_experiment/[stream]
import nim_iterator_stream_experiment/monad/[predicate]
import nim_iterator_stream_experiment/streams/[sequence]

import std/[strutils, sugar]


func spaceAsciiCodes (s: string): seq[uint8] =
  s
    .chars()
    .takeWhile(isAlphaNumeric or isSpaceAscii)
    .filter(isSpaceAscii)
    .map(c => c.uint8)
    .reduce((s: result.typeof(), i: uint8) => s & i, @[])


when isMainModule:
  const
    input = "Hello world\t123 !".repeat(2)
    constCodes = input.spaceAsciiCodes()
  let letCodes = input.spaceAsciiCodes()

  echo(constCodes) # Outputs: @[32, 9, 32]
  doAssert(constCodes == letCodes)
```

### Implementing custom stream operations

To be documented.

### Defining new sources of streams

See the [`streams`](./nim_iterator_stream_experiment/streams) modules.

## To do

- [ ] Reimplement the `mitems` iterator family (or test them if the current API
      already supports them somehow), although the C++ backend has this [issue](
      https://github.com/nim-lang/Nim/issues/10219).
- [ ] Parallelization API.
- [ ] Find a new shorter name for this project.
