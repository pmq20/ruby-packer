# Ruby Packer Changelog

## v2.7.1

- upgrade ruby to 2.7.1
- use internal enclosed Ruby version as rubyc's version prefix
- update license to 2020 and collective authorship

## v0.4.0

- upgrade libsquash to v0.8.0
  - allow creating files inside an existing folder of memfs
    - removes the temporary directory and files at exit
  - produce an executable `squash_sample` when `BUILD_SAMPLE` in cmake
    - let CI discover linking errors earlier about the sample
  - intercept `CreateProcessW`
    - unsets `lpCurrentDirectory` when it was set to `__enclose_io_memfs__` paths
  - intercept `SetCurrentDirectoryW`, `GetCurrentDirectoryW`
  - implement `enclose_io_mkdir` for Windows
    - intercept `_wmkdir`
  - intercept `CreateFileW()` with writing
    - redirect `CreateFileW()` with writing inside the memfs to a temporary directory
    - removes the temporary directory and files at exit
- upgrade to bundler 1.15.3
- fixing the problem of failing to locate bundler
  - https://github.com/pmq20/ruby-packer/issues/11
  - https://github.com/pmq20/ruby-packer/issues/12
- add `--gem` and `--gem-version` to download and compile a gem
- add option --quiet to enable quiet mode

## v0.3.0

- upgrade to bundler 1.15.2
- upgrade to libsquash v0.7.0
  - test ifndef `__USE_XOPEN_EXTENDED`
- upgrade to libautoupdate v0.2.0
  - Auto-update shall only run once in every 24 hours with help of the file `~/.libautoupdate`
  - add argument `force` to `autoupdate()` in order to force an auto-update check
  - add CI to test `autoupdate()`
  - fix failures to replace itself when TMPDIR and current file is not on the same volume
    - https://github.com/pmq20/libautoupdate/issues/1
- use only the master CI
- make it work for projects with gemspecs
- use a temporary directory name with ruby and rubyc version when compiling

## v0.2.0

- upgrade Ruby runtime to 2.4.1
- upgrade libsquash to v0.6.0
- make it work for the 4 samples
  - producing a single Ruby interpreter executable
  - bootstrapping Ruby Compiler itself
  - compiling a CLI tool
  - compiling a Rails application

## v0.1.0

Initial release.
