# TAP Input/Output Test Suite

Definition of input/output tests for `library(date_time)` following the [Test Anything
Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command:

```shell
swipl -q -g main -t halt -s test/test.pl
```

This produces a TAP compatible output like the following:

```prolog
TAP version 13
1..10
ok 1 - parse(date(2021,9,21),4 July,_27172,_27174,_27176) -> _27172=[date(2022,7,4)],_27174=[%d %B],_27176=English
ok 2 - parse(date(2021,9,21),July 4,_27172,_27174,_27176) -> _27172=[date(2022,7,4)],_27174=[%B %d],_27176=English
ok 3 - parse(date(2020,2,28),23 Sep.,_27172,_27174,_27176) -> _27172=[date(2020,9,23)],_27174=[%d %b],_27176=English
ok 4 - parse(date(2021,9,21),4,_27172,_27174,_27176) -> _27172=[date(2021,10,4)],_27174=[%d],_27176=English
ok 5 - parse(date(2020,2,28),Saturday, 2,_27172,_27174,_27176) -> _27172=[date(2020,3,2)],_27174=[%A, %d],_27176=English
ok 6 - parse(date(2021,9,21),Friday, 7. May,_27172,_27174,_27176) -> _27172=[date(2022,5,7)],_27174=[%A, %d %B],_27176=English
ok 7 - parse(date(2021,9,21),saturday, 23 april,_27172,_27174,_27176) -> _27172=[date(2022,4,23)],_27174=[%A, %d %B],_27176=English
ok 8 - parse(date(2020,2,28),23 Sep. - 27 Sep.,_27172,_27174,_27176) -> _27172=[date(2020,9,23),date(2020,9,27)],_27174=[%d %b,%d %b],_27176=English
ok 9 - parse(date(2021,9,21),Today,_27172,_27174,_27176) -> _27172=[date(2021,9,21)],_27174=[today],_27176=English
ok 10 - parse(date(2020,2,29),Tomorrow,_27172,_27174,_27176) -> _27172=[date(2020,3,1)],_27174=[tomorrow],_27176=English

# time=1.5ms
# tests 10
# pass  10
```

## Define Tests

Tests are defined in the `test.pl` file after the call of `:- use_module(library(tap))`.
