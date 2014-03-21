# rebar ct replacement

## Why "topcat"?

Well, it replaces "rebar ct". "ct" reversed is "tc". "T.C." is *Top Cat*.

## What does it give me?

### Progress reporting

It prints out what it's doing as it goes along, so you can better interpret
your test output and results.

### Test summaries

It prints out a nice test summary and list of skipped/failing tests _after_
_all_ your applications have been run.

This means you won't miss a failing test when the output scrolls past, making
it easier to see what's working and not working.

### Working code coverage

It's careful about ensuring that paths are fully-qualified whenever possible.
This means that, in particular, code coverage actually works.

## Including it with rebar.config

Add the following dependency to your top-level rebar.config:

    {topcat, ".*",
      {git, "git://github.com/rlipscombe/topcat.git", {tag, "0.7"}}}

`rebar compile` will build it for you.

Then, where you would have run `rebar ct`, use `deps/topcat/topcat`.

## Usage

### Run everything

    ./deps/topcat/topcat

Runs all suites, groups and tests in all applications.

### Specified application

    ./deps/topcat/topcat -app my_app

Runs all suites, groups and tests from the specified application. You can
specify multiple applications if you prefer.

### Specified test cases

Note that you have to specify the suite as well:

    ./deps/topcat/topcat -app my_app -suite the_suite -case a_test another_test

## Miscellanea

### What's the ".topcat" directory for?

Because of the way that topcat injects itself into the Erlang node that's
running your Common Test suites, it needs to provide a set of `.beam` files to
the Erlang runtime.

These are packaged into the top-level `topcat` escript, but need to be
extracted for the Erlang runtime to find them later.

They go into the `.topcat` folder. You should add it to your `.gitignore` file
and to your `make clean` target. Other than that, feel free to ignore it.

## Licensing

Apache 2.0

