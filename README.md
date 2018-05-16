# Ruby Packer

*Packing your Ruby application into a single executable.*

[![Status](https://ci.appveyor.com/api/projects/status/93i36eliiy6v3686/branch/master?svg=true)](https://ci.appveyor.com/project/pmq20/ruby-compiler/branch/master)
[![Status](https://travis-ci.org/pmq20/ruby-compiler.svg?branch=master)](https://travis-ci.org/pmq20/ruby-compiler)
[![GitHub version](https://badge.fury.io/gh/pmq20%2Fruby-compiler.svg)](https://badge.fury.io/gh/pmq20%2Fruby-compiler)

## Features

- Works on Linux, Mac and Windows
- Creates a binary distribution of your application
- Supports natively any form of `require` and `load`, including dynamic ones (e.g. `load(my_path + 'x.rb'`)
- Features zero-config auto-update capabilities to make your compiled project to stay updated
- Native C extensions are fully supported
- Rails applications are fully supported
- Open Source, MIT Licensed

## Get Started

It takes less than 5 minutes to compile any project with Ruby Compiler.

You won't need to modify a single line of code in your application, no matter how you developed it as long as it works in plain Ruby!

|                       | Architecture |           Latest&#160;Stable                 |
|:---------------------:|:------------:|----------------------------------------------|
|       **macOS**       |    x86-64    | http://enclose.io/rubyc/rubyc-darwin-x64.gz  |
|       **Linux**       |    x86-64    | http://enclose.io/rubyc/rubyc-linux-x64.gz   |
|      **Windows**      |    x86-64    | http://enclose.io/rubyc/rubyc-x64.zip        |

For previous releases, cf. http://enclose.io/rubyc

### Install on macOS

First install the prerequisites:

* [SquashFS Tools 4.3](http://squashfs.sourceforge.net/): `brew install squashfs`
* [Xcode](https://developer.apple.com/xcode/download/)
  * You also need to install the `Command Line Tools` via Xcode. You can find
    this under the menu `Xcode -> Preferences -> Downloads`
  * This step will install `gcc` and the related toolchain containing `make`
* [Ruby](https://www.ruby-lang.org/)

Then,

    curl -L http://enclose.io/rubyc/rubyc-darwin-x64.gz | gunzip > rubyc
    chmod +x rubyc
    ./rubyc --help

### Install on Linux

First install the prerequisites:

* [SquashFS Tools 4.3](http://squashfs.sourceforge.net/)
  - `sudo yum install squashfs-tools`
  - `sudo apt-get install squashfs-tools`
* `gcc` or `clang`
* GNU Make
* [Ruby](https://www.ruby-lang.org/)

Then,

    curl -L http://enclose.io/rubyc/rubyc-linux-x64.gz | gunzip > rubyc
    chmod +x rubyc
    ./rubyc --help

### Install on Windows

First install the prerequisites:

* [SquashFS Tools 4.3](https://github.com/pmq20/squashfuse/files/691217/sqfs43-win32.zip)
* [Visual Studio 2015 Update 3](https://www.visualstudio.com/), all editions
  including the Community edition (remember to select
  "Common Tools for Visual C++ 2015" feature during installation).
* [Ruby](https://www.ruby-lang.org/)

Then download [rubyc-x64.zip](http://enclose.io/rubyc/rubyc-x64.zip),
and this zip file contains only one executable.
Unzip it. Optionally,
rename it to `rubyc.exe` and put it under `C:\Windows` (or any other directory that is part of `PATH`).
Execute `rubyc --help` from the command line.

## Usage

If ENTRANCE was not provided, then a single Ruby interpreter executable will be produced.
ENTRANCE can be either a file path, or a "x" string as in bundle exec "x".

    rubyc [OPTION]... [ENTRANCE]
      -r, --root=DIR                   The path to the root of the application
      -o, --output=FILE                The path of the output file
      -d, --tmpdir=DIR                 The directory for temporary files
      -c, --clean-tmpdir               Cleans temporary files before compiling
          --keep-tmpdir                Keeps all temporary files that were generated last time
          --openssl-dir                The path to openssl
          --make-args=ARGS             Extra arguments to be passed to make
          --nmake-args=ARGS            Extra arguments to be passed to nmake
          --auto-update-url=URL        Enables auto-update and specifies the URL to get the latest version
          --auto-update-base=STRING    Enables auto-update and specifies the base version string
          --debug                      Enable debug mode
      -v, --version                    Prints the version of rubyc and exit
          --ruby-version               Prints the version of the Ruby runtime and exit
          --ruby-api-version           Prints the version of the Ruby API and exit
      -h, --help                       Prints this help and exit

### Openssl

rubyc compiles its own version of openssl without any certifications.
To be able to use ssl with rubyc it should know where to find the certifications.

By default this path is set to `/usr/local/etc/openssl/` but can be overridden using the `--openssl-dir` argument.

Keep in mind that users running your compiled package should have their certifications
present in this directory as well.

## Examples

### Producing a single Ruby interpreter executable

	rubyc
	./a.out (or a.exe on Windows)

### Compiling a CLI tool

	git clone --depth 1 https://github.com/pmq20/ruby-compiler
	cd ruby-compiler
	rubyc bin/rubyc
	./a.out (or a.exe on Windows)

### Compiling a Rails application

	rails new yours
	cd yours
	rubyc bin/rails
	./a.out server (or a.exe server on Windows)

Note that some gems that use C extensions that use libc IO to load files from
your Rails application will not work with rubyc.  Notably, [bootsnap will not
work with rubyc](https://github.com/pmq20/ruby-packer/issues/30#issuecomment-387893082).

### Compiling a Gem

	rubyc --gem=bundler --gem-version=1.15.4 bundle
	./a.out (or a.exe on Windows)

## Building rubyc yourself

To build `rubyc` you must have a C compiler and the necessary toolchain to
build ruby and the libraries stuffed inside rubyc which include at least:
* gdbm
* libffi
* ncurses
* openssl
* readline
* yaml
* zlib

If you are unsure if your toolchain is complete then trying to build `rubyc`
will let you know you are missing something.  Unfortunately it may tell you
with some unfamiliar message.  Please file an issue here if this occurs.

Once your toolchain is set up run `bundle`.  To compile your own `rubyc` run:

	bundle exec rake rubyc

Or:

	rm rubyc; ruby -Ilib bin/rubyc bin/rubyc -o rubyc

Remember that rubyc includes all the files from the current directory in the
built executable.  You must *delete the prior rubyc* or your squashfs will
*continually grow larger* and the embedded squashfs *compile time will be
very, very long*.

If you make changes to the stuffed libraries or the compiler you may need to
add the `--clean-tmpdir` argument to `rubyc` for a clean rebuild.

## See Also

- [Libsquash](https://github.com/pmq20/libsquash): portable, user-land SquashFS that can be easily linked and embedded within your application.
- [Libautoupdate](https://github.com/pmq20/libautoupdate): cross-platform C library to enable your application to auto-update itself in place.
