# Ruby Compiler Changelog

## v0.4.0



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
