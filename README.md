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
[new](prolog/abbreviated_dates.plt) might give an impression on how to use this library.

## Requirements

Only for development purposes the [`date_time` pack](http://www.swi-prolog.org/pack/list?p=date_time) is needed:

```prolog
?- pack_install(date_time).
```
For maintenance tasks [bumpversion](https://github.com/peritus/bumpversion) and [hub](https://github.com/github/hub) 
might be required.

### Run Tests

The old tap & new tests can be run using the following command:

```shell
make test
```

### Define new Tests

New tests should be defined in the [test](prolog/abbreviated_dates.plt) file.

## Review
https://eu.swi-prolog.org/pack/review?p=abbreviated_dates