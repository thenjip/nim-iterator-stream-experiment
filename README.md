# nim-iterator-stream-experiment

[![Build Status](https://github.com/thenjip/nim-iterator-stream-experiment/workflows/Unit%20tests/badge.svg?branch=master)](https://github.com/thenjip/nim-iterator-stream-experiment/actions?query=workflow%3A"Unit+tests"+branch%3A"master")
[![licence](https://img.shields.io/github/license/thenjip/nim-iterator-stream-experiment.svg)](https://raw.githubusercontent.com/thenjip/nim-iterator-stream-experiment/master/LICENSE)

An attempt at working around [first class iterators](https://nim-lang.org/docs/manual.html#iterators-and-the-for-statement-first-class-iterators)
in [Nim](https://nim-lang.org/) with an API similar to Java 8 Stream and aiming
at maximum backend compatibility.

## Installation

```sh
nimble install 'https://github.com/thenjip/nim-iterator-stream-experiment'
```

### Dependencies

`nim` >= `1.2.0`

## Documentation

[Read the docs](https://thenjip.github.io/nim-iterator-stream-experiment).

## Rationale

First class iterators are not available in Nim's VM and JavaScript backends.
This project exists to see if implementing an iterator API compatible with all
Nim's backends is possible (including compile time execution).

First class iterators are the priority, but it turns out any iterator that does
not rely on compiler magic can be replaced (example: the [iterator](
https://nim-lang.org/docs/iterators.html#fields.i%2CT) on structure fields).

The API is inspired by Java 8 Stream but the implementation is almost entirely
based on functional programming.

Many types used to implement this API rely on closure procedures. So they can
not be assigned to `const` symbols, but they can still be part of computations
at compile time.

## Backend compatibility

The results below are based on the status of CI builds.

| Backend | @ Run time | @ Compile time |
| :--- | :--- | :--- |
| C | Supported | Supported |
| C++ | Supported | Supported |
| JavaScript | Supported | Unsupported.<p></p>Compile time tests are currently skipped, they do not compile yet. See [doc](https://thenjip.github.io/nim-iterator-stream-experiment/nim_iterator_stream_experiment/stream.html#backend-restrictions-javascript). |
| NimScript | Limited support.<p></p>See [doc](https://thenjip.github.io/nim-iterator-stream-experiment/nim_iterator_stream_experiment/stream.html#backend-restrictions-nimscript). | Same as "@ run time". |

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

The current public API may not be enough to implement a custom stream operation.
It may need to modify one or more parts of the stream [structure](https://thenjip.github.io/nim-iterator-stream-experiment/nim_iterator_stream_experiment/stream.html#Stream).

The object types in the library do not expose their members,
but [lenses](https://thenjip.github.io/nim-iterator-stream-experiment/nim_iterator_stream_experiment/optics/plens.html) on each member are available.

The library internally uses these lenses when:

- A direct access to a member is not possible.
- Modifying a member through a lens is more convenient than rebuilding the whole
  structure with the updated member, which can become even worse with structures
  inside other ones.

Examples:

- [map](https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/nim_iterator_stream_experiment/stream.nim#L325-L326)
- [dropWhile](https://github.com/thenjip/nim-iterator-stream-experiment/blob/ccf6d7bd1096088a5de26fc8fd670a3fc3dd1e50/nim_iterator_stream_experiment/stream.nim#L450-L463)
- [filter](https://github.com/thenjip/nim-iterator-stream-experiment/blob/ccf6d7bd1096088a5de26fc8fd670a3fc3dd1e50/nim_iterator_stream_experiment/stream.nim#L329-L334)
- [limit](https://github.com/thenjip/nim-iterator-stream-experiment/blob/ccf6d7bd1096088a5de26fc8fd670a3fc3dd1e50/nim_iterator_stream_experiment/stream.nim#L344-L362)

### Defining new sources of streams

See the [`streams`](nim_iterator_stream_experiment/streams) modules for
examples.

## To do

- [ ] Give the ability to provide an in-place stepper. This would require
      `var T` return types as well as the `var T from x` syntax
      [idea](https://nim-lang.org/docs/manual.html#var-return-type-future-directions).
- [ ] Reimplement the `mitems` iterator family (or test them if the current API
      already supports them somehow), although the C++ backend has this
      [issue](https://github.com/nim-lang/Nim/issues/10219).
- [ ] Parallelization API.
- [ ] Find a new shorter name for this project.
