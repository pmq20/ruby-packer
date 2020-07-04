# Ruby Packer

*Packing your Ruby application into a single executable.*

[![Windows](https://github.com/pmq20/ruby-packer/workflows/Windows/badge.svg)](https://github.com/pmq20/ruby-packer/actions?query=workflow%3A"Windows")
[![macOS](https://github.com/pmq20/ruby-packer/workflows/macOS/badge.svg)](https://github.com/pmq20/ruby-packer/actions?query=workflow%3A"macOS")
[![Linux](https://github.com/pmq20/ruby-packer/workflows/Linux/badge.svg)](https://github.com/pmq20/ruby-packer/actions?query=workflow%3A"Linux")

> **NEWS (Jul 4th, 2020)**: We have decided to use GitHub Actions and GitHub Releases as the auto-packing and auto-updating web services. The Latest Build links below are automatically kept up to date with the latest code in `master`, as long as all the CI tests are passed.

> **NEWS (Jun 27th, 2020)**: Due to work, the project's original author [Minqi Pan](https://github.com/pmq20) is currently only available to maintain the project during his weekend. Also, we are attending to reported issues one by one, ordered by creation time descendingly.

## Features

- Works on ![win](resource/win_sm.png) Windows, ![macOS](resource/apple_sm.png) macOS and ![linux](resource/linux_sm.png) Linux
- Windows support is implemented via native Windows API's; MSYS2/MinGW/Cygwin are NOT depended upon
- Creates a binary distribution of your Ruby and/or Rails application
- Supports natively any form of `require` and `load`, including dynamic ones (e.g. `load(my_path + '/x.rb')`)
- Features zero-config auto-update capabilities to make your packed project to stay up to date
- Native C extensions are fully supported
- Open Source, MIT Licensed

## Download and Install

It takes less than 5 minutes to compile any project with Ruby Packer.

You won't need to modify a single line of code in your application, no matter how you developed it as long as it works in plain Ruby!

|               | Arch. |                               Latest Build                                    |
|:-------------:|:-----:|-------------------------------------------------------------------------------|
|  **Windows**  |  x64  | https://github.com/pmq20/ruby-packer/releases/download/windows-x64/rubyc.exe  |
|   **macOS**   |  x64  | https://github.com/pmq20/ruby-packer/releases/download/darwin-x64/rubyc       |
|   **Linux**   |  x64  | https://github.com/pmq20/ruby-packer/releases/download/linux-x64/rubyc        |

### ![win](resource/win_med.png) Install on Windows

First install the prerequisites:

* [SquashFS Tools](http://squashfs.sourceforge.net/): you could install it by first installing [choco](https://chocolatey.org) and then executing `choco install squashfs`.
* [Visual Studio](https://www.visualstudio.com/), all editions including the Community edition (remember to select "Common Tools for Visual C++" feature during installation).
* [Ruby](https://www.ruby-lang.org/): you could install it using [RubyInstaller](https://rubyinstaller.org/).

Then download [rubyc.exe](https://github.com/pmq20/ruby-packer/releases/download/windows-x64/rubyc.exe).
Optionally put it under `C:\Windows` or any other directory that is part of `PATH`.
Execute `rubyc --help` from the command line.

### ![macOS](resource/apple_med.png) Install on macOS

First install the prerequisites:

* [SquashFS Tools](http://squashfs.sourceforge.net/): `brew install squashfs`
* [Xcode](https://developer.apple.com/xcode/download/)
  * You also need to install the `Command Line Tools` via Xcode. You can find
    this under the menu `Xcode -> Preferences -> Downloads`
  * This step will install `gcc` and the related toolchain containing `make`
* [Ruby](https://www.ruby-lang.org/)

Then,

    curl -OL https://github.com/pmq20/ruby-packer/releases/download/darwin-x64/rubyc
    chmod +x rubyc
    ./rubyc --help

### ![linux](resource/linux_med.png) Install on Linux

First install the prerequisites:

* [SquashFS Tools](http://squashfs.sourceforge.net/)
  - `sudo yum install squashfs-tools`
  - `sudo apt install squashfs-tools`
* `gcc` or `clang`
* GNU Make
* [Ruby](https://www.ruby-lang.org/)

Then,

    curl -OL https://github.com/pmq20/ruby-packer/releases/download/linux-x64/rubyc
    chmod +x rubyc
    ./rubyc --help

## Usage

If ENTRANCE was not provided, then a single portable Ruby interpreter executable will be produced.
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
      -i, --ignore-file                Ignore file(s) from build
      -v, --version                    Prints the version of rubyc and exit
          --ruby-version               Prints the version of the Ruby runtime and exit
          --ruby-api-version           Prints the version of the Ruby API and exit
      -h, --help                       Prints this help and exit

### The `--openssl-dir` Option

rubyc compiles its own version of openssl without any certifications.
To be able to use ssl with rubyc it should know where to find the certifications.

By default this path is set to `/usr/local/etc/openssl/` but can be overridden using the `--openssl-dir` argument.

Keep in mind that users running your compiled package should have their certifications
present in this directory as well.

### The `--ignore-file` Option

If you don't want certain files included in the build you can ignore them from the command line using -i.

	rubyc -i ignore.file -i ignore-2.file -i "ignore*"

Alternatively you can create a `.rubycignore` file in the root of your project to specify which files should be ignored.

## Examples

### Producing a Portable Ruby Interpreter Executable

I.e. packing the raw Ruby interpreter without packing any projects:

	rubyc
	./a.out (or a.exe on Windows)

### Packing a CLI utility

Taking Ruby Packer itself as an example of the CLI utility to pack:

	git clone --depth 1 https://github.com/pmq20/ruby-packer
	cd ruby-packer
	rubyc bin/rubyc
	./a.out (or a.exe on Windows)

### Packing a Gem

	rubyc --gem=bundler --gem-version=1.15.4 bundle
	./a.out (or a.exe on Windows)

Note that some gems that use C extensions that use libc IO to load files from
your Rails application will not work with rubyc.  Notably, [bootsnap will not
work with rubyc](https://github.com/pmq20/ruby-packer/issues/30#issuecomment-387893082).

### Packing a Rails Application

	rails new yours
	cd yours
	rubyc bin/rails
	./a.out server (or a.exe server on Windows)

## Building `rubyc` from Source

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

Or (if you want to compile with debug symbols):

  ENCLOSE_IO_RUBYC_ADDTIONAL_ARGS=--debug bundle exec rake rubyc

This will produce a single `rubyc` executable, which can
be put inside any of your `PATH` locations, so that it can be directly
called from the command prompt. For example:

	mv rubyc /usr/local/bin

Remember that rubyc includes all the files from the current directory in the
built executable.  You must *delete the prior rubyc* or your squashfs will
*continually grow larger* and the embedded squashfs *compile time will be
very, very long*.

If you make changes to the stuffed libraries or the compiler you may need to
add the `--clean-tmpdir` argument to `rubyc` for a clean rebuild.

## Authors

[Minqi Pan et al](https://raw.githubusercontent.com/pmq20/ruby-packer/master/AUTHORS)

## License

[MIT License](https://raw.githubusercontent.com/pmq20/ruby-packer/master/LICENSE)

## See Also

- [RubyConf 2017 (New Orleans, LA) presentation video: Packing your Ruby application into a single executable](https://www.youtube.com/watch?v=1mme7HiLqzA).
- [Libsquash](https://github.com/pmq20/libsquash): portable, user-land SquashFS that can be easily linked and embedded within your application.
- [Libautoupdate](https://github.com/pmq20/libautoupdate): cross-platform C library to enable your application to auto-update itself in place.
- [Squashfs Tools](https://github.com/plougher/squashfs-tools): tools to create and extract Squashfs filesystems.
