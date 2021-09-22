# TAP Input/Output Test Suite

Definition of input/output tests for `library(date_time)` following the [Test Anything
Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command:

```shell
swipl -q -g main -t halt -s test/test.pl
```

This produces a TAP compatible output like the following:

```
TAP version 13
1..2
ok 1 - parse(date(2021,9,21),Freitag, 7. Mai,_16448,_16450) -> _16448=[date(2022,5,7)],_16450=[sd(wd(German),dm(explicit(German)))]
ok 2 - parse(date(2021,9,21),23 Sze. - 27 Sze.,_16448,_16450) -> _16448=[date(2021,9,23),date(2021,9,27)],_16450=[dm(abbreviated(Hungarian)),dm(abbreviated(Hungarian))]

# time=5.6ms
# tests 2
# pass  2
```

## Define Tests

Tests are defined in the `test.pl` file after the call of `:- use_module(library(tap))`.
