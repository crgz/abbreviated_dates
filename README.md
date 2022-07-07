<img src=".github/flags-jakearchibald.github.io-scour.svg?raw=true" width="50%" align="right" style="border:20px solid white">

# Abbreviated Date Parser Library

The Abbreviated Date Parser Library contains a predicate that parses abbreviated incomplete dates in multiple languages.

```prolog 
parse(Context, Expression, Dates, Trace).
```

## Installation

This pack is available from the [add-on registry of SWI-Prolog](http://www.swi-prolog.org/pack/list).

It can be installed with `pack_install/1`:

```prolog
?- pack_install(abbreviated_dates).
```

Then, you can use it by simply calling `use_module(library(abbreviated_dates))`.

## Provided Predicates

```prolog
parse/4.  % parse an abbreviated incomplete date in multiple languages (today, tomorrow, etc).
```

For further details have a look at the [implementation](prolog/abbreviated_dates.pl). In addition, the
[new](prolog/abbreviated_dates.plt) and [old](test/test.pl) might give an impression on how to use this library.

## Requirements

Only for development purposes the [`tap` pack](http://www.swi-prolog.org/pack/list?p=tap) is needed:

```prolog
?- pack_install(tap).
```
For maintenance tasks [bumpversion](https://github.com/peritus/bumpversion) and [hub](https://github.com/github/hub) might be required.

## Test Suite

We are migrating the test suit from tap to PL-Unit aiming to implement test coverage metrics.

### Run Tests

The new tests can be run using the following command:

```shell
swipl -t "load_test_files([]), run_tests." prolog/abbreviated_dates.pl
```shell

The tap tests can be run using the following command:

```shell
swipl -q -g main -t halt -s test/test.pl
```

### Define new Tests

New tests should be defined in the [new test](prolog/abbreviated_dates.plt) file.