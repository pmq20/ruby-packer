# Ruby Compiler

Compiler for Ruby which compiles your project into a single executable.

[![Linux and Mac OS X Build Status](https://travis-ci.org/pmq20/ruby-compiler.svg?branch=master)](https://travis-ci.org/pmq20/ruby-compiler)

## Installation

    gem install ruby-compiler

You might need to `sudo` if prompted with no-permission errors.

## Usage

    rubyc [OPTION]... [ENTRANCE]
        -p, --project-root=DIR           The path to the root of the project (default: .)
        -o, --output=FILE                The path of the output file (default: ./a.out or ./a.exe)
        -d, --tmpdir=DIR                 The directory for temporary files
            --make-args=ARGS             Extra arguments to be passed to make
            --nmake-args=ARGS            Extra arguments to be passed to nmake
        -v, --version                    Prints the version of rubyc and exit
            --ruby-version               Prints the version of the Ruby runtime and exit
            --ruby-api-version           Prints the version of the Ruby API and exit
        -h, --help                       Prints this help and exit

## Examples

### Compiling a CLI project

    git clone https://github.com/pmq20/ruby-compiler.git
    cd ruby-compiler
    rubyc rubyc

### Compiling a Rails project

    git clone https://github.com/ruby-china/homeland.git
    cd homeland
    rubyc rails

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. Or without installing, run `bundle exec rubyc` from the root of your project directory.

To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/pmq20/ruby-compiler.

## License

Copyright (c) 2016-2017 **Minqi Pan** &lt;pmq2001@gmail.com&gt;, under terms of the [MIT License](http://opensource.org/licenses/MIT).

