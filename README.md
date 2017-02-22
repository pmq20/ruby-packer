# Ruby Compiler

Compiling your Ruby application into a single executable.

http://enclose.io

[![Travis CI status](https://travis-ci.org/pmq20/ruby-compiler.svg?branch=master)](https://travis-ci.org/pmq20/ruby-compiler)
[![AppVeyor status](https://ci.appveyor.com/api/projects/status/93i36eliiy6v3686/branch/master?svg=true)](https://ci.appveyor.com/project/pmq20/ruby-compiler/branch/master)

## Download

|    Operating System   |  Architecture | Download Link                                                                          |
|:---------------------:|:-------------:|----------------------------------------------------------------------------------------|
|        Windows        |      x86      | https://sourceforge.net/projects/ruby-compiler/files/v0.1.0/rubyc.exe/download         |
|         macOS         |     x86-64    | https://sourceforge.net/projects/ruby-compiler/files/v0.1.0/rubyc-darwin-x64/download  |
|         Linux         |     x86-64    | https://sourceforge.net/projects/ruby-compiler/files/v0.1.0/rubyc-linux-x64/download   |

## Install

### Windows

First install the prerequisites:

* [SquashFS Tools 4.3](https://github.com/pmq20/squashfuse/files/691217/sqfs43-win32.zip)
* [Visual Studio 2010](https://www.visualstudio.com/) or newer
* [Ruby 2.4.0](https://github.com/pmq20/rubyinstaller/files/689117/rb240-win32.zip)
  * make sure `require 'socket'` works, if not install the socket extension
  * make sure `require 'zlib'` works, if not install the zlib extension

Then download the executable `rubyc.exe` and run it from the Visual Studio Command Prompt.

### macOS

First install the prerequisites:

* [SquashFS Tools 4.3](http://squashfs.sourceforge.net/): `brew install squashfs`
* [Xcode](https://developer.apple.com/xcode/download/)
  * You also need to install the `Command Line Tools` via Xcode. You can find
    this under the menu `Xcode -> Preferences -> Downloads`
  * This step will install `gcc` and the related toolchain containing `make`
* [Ruby 2.4.0](https://www.ruby-lang.org/)

Then,

    curl -L https://sourceforge.net/projects/ruby-compiler/files/v0.1.0/rubyc-darwin-x64/download > rubyc
    chmod +x rubyc
    ./rubyc

### Linux

First install the prerequisites:

* [SquashFS Tools 4.3](http://squashfs.sourceforge.net/)
* `gcc` or `clang`
* GNU Make
* [Ruby 2.4.0](https://www.ruby-lang.org/)

Then,

    curl -L https://sourceforge.net/projects/ruby-compiler/files/v0.1.0/rubyc-linux-x64/download > rubyc
    chmod +x rubyc
    ./rubyc

## Usage

    rubyc [OPTION]... [ENTRANCE]
      -r, --root=DIR                   The path to the root of the application
      -o, --output=FILE                The path of the output file
      -d, --tmpdir=DIR                 The directory for temporary files
          --make-args=ARGS             Extra arguments to be passed to make
          --nmake-args=ARGS            Extra arguments to be passed to nmake
          --debug                      Enable debug mode
      -v, --version                    Prints the version of rubyc and exit
          --ruby-version               Prints the version of the Ruby runtime and exit
          --ruby-api-version           Prints the version of the Ruby API and exit
      -h, --help                       Prints this help and exit

## Example

    git clone --depth 1 https://github.com/pmq20/ruby-compiler.git
    cd ruby-compiler
    rubyc bin/rubyc
    ./a.out (or a.exe on Windows)

## See Also

- [Libsquash](https://github.com/pmq20/libsquash): portable, user-land SquashFS that can be easily linked and embedded within your application.
- [SquashFS](http://squashfs.sourceforge.net/): a compressed read-only filesystem for Linux.
