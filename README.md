# Ruby Compiler

Compiling your Ruby application into a single executable.

http://enclose.io

[![Travis CI status](https://travis-ci.org/pmq20/ruby-compiler.svg?branch=master)](https://travis-ci.org/pmq20/ruby-compiler)
[![AppVeyor status](https://ci.appveyor.com/api/projects/status/93i36eliiy6v3686/branch/master?svg=true)](https://ci.appveyor.com/project/pmq20/ruby-compiler/branch/master)

## Nightly Builds: Feb 13, 2017

| Operating System | Architecture | URL                                           |
|:----------------:|:------------:|-----------------------------------------------|
|      Windows     |      x86     | http://enclose.io/2017feb13/rubyc.exe         |
|       macOS      |     x86-64   | http://enclose.io/2017feb13/rubyc-darwin-x64  |
|       Linux      |     x86-64   | http://enclose.io/2017feb13/rubyc-linux-x64   |

On Windows, you could just download Ruby Compiler `rubyc.exe` and run it from the Visual Studio command line.

On macOS, you could install Ruby Compiler like this:

    sudo curl http://enclose.io/2017feb13/rubyc-darwin-x64 > /usr/local/bin/rubyc
    chmod +x /usr/local/bin/rubyc
    rubyc

On Linux, you could install Ruby Compiler like this:

    sudo curl http://enclose.io/2017feb13/rubyc-linux-x64 > /usr/local/bin/rubyc
    chmod +x /usr/local/bin/rubyc
    rubyc

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
