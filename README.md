# Ruby Compiler

Compiling your Ruby application into a single executable.

http://rubyc.enclose.io

[![Travis CI status](https://travis-ci.org/pmq20/ruby-compiler.svg?branch=master)](https://travis-ci.org/pmq20/ruby-compiler)
[![AppVeyor status](https://ci.appveyor.com/api/projects/status/93i36eliiy6v3686/branch/master?svg=true)](https://ci.appveyor.com/project/pmq20/ruby-compiler/branch/master)

## Download

| Operating System | Architecture | Link                                                           |
|:----------------:|:------------:|----------------------------------------------------------------|
|      Windows     |      x86     | http://enclose.io/pmq20/ruby-compiler/rubyc-master.exe         |
|       macOS      |     x86-64   | http://enclose.io/pmq20/ruby-compiler/rubyc-master-darwin-x64  |
|       Linux      |     x86-64   | http://enclose.io/pmq20/ruby-compiler/rubyc-master-linux-x64   |

On macOS and Linux, you need to execute `chmod +x` on the downloaded file.

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
