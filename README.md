# Tagua VM

[![Build status](https://api.travis-ci.org/tagua-vm/parser.svg?branch=master)](https://travis-ci.org/tagua-vm/parser)
[![Chat on Freenode](https://img.shields.io/badge/chat-on_%23taguavm-ff0066.svg)](https://webchat.freenode.net/?channels=#taguavm)
[![Chat on Gitter](https://img.shields.io/badge/chat-on_gitter-ff0066.svg)](https://gitter.im/tagua-vm/tagua-vm)

Tagua VM is an experimental [PHP](http://php.net/) Virtual Machine written with
[the Rust language](https://www.rust-lang.org/) and [the LLVM Compiler
Infrastructure](http://llvm.org/).

The Virtual Machine is composed of several libraries. This library contains the
lexical and syntactic analysers, aka the parser, for the PHP language.

## Installing

To install Tagua VM, you must have Rust (see [the Rust installation
page](https://www.rust-lang.org/downloads.html)) installed.
[Cargo](http://doc.crates.io/guide.html) is the Rust package manager.

To build a release version:

```sh
$ cargo build --release
```

To build a development version:

```sh
$ cargo build
```

### Using Docker

If installing Rust and LLVM on your machine is too much, Docker might be an
alternative: It provides everything needed to build, test and run Tagua VM.

First, build the Docker image:

```sh
$ docker build -t tagua-vm-parser-dev
```

Now, it is possible to run a container from this image:

```sh
$ docker run --rm -it -v $(pwd):/source tagua-vm-parser-dev
```

If this command succeeds, you are inside a fresh container. To see if
everything is fine, you can start the test suite:

```sh
$ cargo test
```

## Contributing

Do whatever you want. Just respect the license and the other contributors. Your
favorite tool is going to be:

```sh
$ cargo test
```

to run all the test suites (unit test suites, integration test suites and
documentation test suites).

### カンバン ([Kanban](https://en.wikipedia.org/wiki/Kanban))

In order to get an overview of what needs to be done, what is in progress and
what has been recently done, [a kanban board is
available](https://waffle.io/tagua-vm/parser).

## Documentation

The documentation is not online yet. To generate it locally, please, run the
following command:

```sh
$ cargo doc
$ open target/doc/taguavm_parser/index.html
```

To get help on IRC, please join the official [`#taguavm` channel on
Freenode](https://webchat.freenode.net/?channels=#taguavm). Alternatively, there
is a [mirrored room on Gitter](https://gitter.im/tagua-vm/tagua-vm).

## License

Tagua VM is under the New BSD License (BSD-3-Clause):

```
                                New BSD License



Copyright © 2016-2016, Ivan Enderlin.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Hoa nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
```
