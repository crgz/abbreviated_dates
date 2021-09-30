<img src=".github/World-Flag-Buttons-Globe.svgz?raw=true" width="50%" align="right" style="border:20px solid white">

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

## Requirements

Only for development purposes the [`tap` pack](http://www.swi-prolog.org/pack/list?p=tap) is needed:

```prolog
?- pack_install(tap).
```

## Provided Predicates

```prolog
parse/4.  % parse an abbreviated incomplete date in multiple languages (today, tomorrow, etc).
```

For further details have a look at the [implementation](prolog/abbreviated_dates.pl). In addition, the [defined
tests](test/test.pl) might give an impression on how to use this library.
