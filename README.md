# Ruby Packer

*Packing your Ruby application into a single executable.*

[![Status](https://ci.appveyor.com/api/projects/status/93i36eliiy6v3686/branch/master?svg=true)](https://ci.appveyor.com/project/pmq20/ruby-packer/branch/master)
[![Status](https://travis-ci.org/pmq20/ruby-packer.svg?branch=master)](https://travis-ci.org/pmq20/ruby-packer)

## Features

It takes less than 5 minutes to pack any project with Ruby Packer. You won't need to modify a single line of code in your application, no matter how you developed it as long as it works in plain Ruby!

- Works on Linux, Mac and Windows
- Creates a binary distribution of your application
- Supports natively any form of `require` and `load`, including dynamic ones (e.g. `load(my_path + 'x.rb'`)
- Features zero-config auto-update capabilities to make your packed project to stay up to date
- Native C extensions are fully supported
- Rails applications are fully supported
- Open Source, MIT Licensed

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
      -i, --ignore-file                Ignore file(s) from build
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

### Ignore files

If you don't want certain files included in the build you can ignore them from the command line using -i.

  rubyc -i ignore.file -i ignore-2.file -i "ignore*"

Alternatively you can create a `.rubycignore` file in the root of your project to specify which files should be ignored.

## Examples

### Packing a CLI tool (E.g. Ruby Packer itself)

	git clone --depth 1 https://github.com/pmq20/ruby-packer
	cd ruby-packer
	rubyc bin/rubyc
	./a.out (or a.exe on Windows)

### Packing a Gem

	git clone --depth 1 https://github.com/pmq20/ruby-packer
	cd ruby-packer
	bin/rubyc --gem=bundler --gem-version=1.15.4 bundle
	./a.out (or a.exe on Windows)

Note that some gems that use C extensions that use libc IO to load files from
your Rails application will not work with rubyc.  Notably, [bootsnap will not
work with rubyc](https://github.com/pmq20/ruby-packer/issues/30#issuecomment-387893082).

### Packing a Rails Application

	rails new yours
	cd yours
	rubyc bin/rails
	./a.out server (or a.exe server on Windows)

### Producing a Single Ruby interpreter Executable (I.e. Raw Ruby without your Project)

	git clone --depth 1 https://github.com/pmq20/ruby-packer
	cd ruby-packer
	rubyc
	./a.out (or a.exe on Windows)

## Building `rubyc`

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

### Building `rubyc` on macOS

First install the prerequisites:

* [SquashFS Tools 4.3](http://squashfs.sourceforge.net/): `brew install squashfs`
* [Xcode](https://developer.apple.com/xcode/download/)
  * You also need to install the `Command Line Tools` via Xcode. You can find
    this under the menu `Xcode -> Preferences -> Downloads`
  * This step will install `gcc` and the related toolchain containing `make`
* [Ruby](https://www.ruby-lang.org/)


### Building `rubyc` on Linux

First install the prerequisites:

* [SquashFS Tools 4.3](http://squashfs.sourceforge.net/)
  - `sudo yum install squashfs-tools`
  - `sudo apt install squashfs-tools`
* `gcc` or `clang`
* GNU Make
* [Ruby](https://www.ruby-lang.org/)

### Building `rubyc` on Windows

First install the prerequisites:

* [SquashFS Tools 4.3](https://github.com/pmq20/squashfuse/files/691217/sqfs43-win32.zip)
* [Visual Studio 2015 Update 3](https://visualstudio.microsoft.com/vs/older-downloads/), all editions
  including the Community edition (remember to select
  "Common Tools for Visual C++ 2015" feature during installation).
* [Bison for Windows](http://gnuwin32.sourceforge.net/packages/bison.htm).  When installing, make sure
  to select the binaries and developer files.  Do NOT install to the default C:\Program Files location.
  Choose a location without spaces, such as C:\Gnuwin32.
  If you encounter problems related to bison later in your installation, you may want to consider overwriting
  bison.exe with this [patched Windows binary](http://marin.jb.free.fr/bison/).
* [Sed for Windows](http://gnuwin32.sourceforge.net/packages/sed.htm).  When installing, make sure
  to select the binaries and developer files.  Do NOT install to the default C:\Program Files location.
  Choose a location without spaces, such as C:\Gnuwin32.
* [Ruby](https://www.ruby-lang.org/)

From a command prompt window, load the Visual Studio environment variables for 32-bit compilation.  By default,
this is located at c:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\vcvars32.bat.

Ensure that bison and sed are in your path. If you installed to C:\Gnuwin32, they will be in C:\Gnuwin32\bin.

## See Also

- [Libsquash](https://github.com/pmq20/libsquash): portable, user-land SquashFS that can be easily linked and embedded within your application.
- [Libautoupdate](https://github.com/pmq20/libautoupdate): cross-platform C library to enable your application to auto-update itself in place.
- [squashfs-tools](https://github.com/plougher/squashfs-tools): tools to create and extract Squashfs filesystems.
