<img src="https://raw.githubusercontent.com/crgz/abbreviated_dates/286a20f7c46525c3e29af6137840db1b247c23f9/.github/images/logo.svg" width="25%" align="right" style="border:0px solid white">

<h3 align="center">Parser for Abbreviated Dates</h3>

<p align="center">
    <a href=https://www.swi-prolog.org/pack/list?p=abbreviated_dates">
        <img src="https://github.com/crgz/abbreviated_dates/blob/main/.github/badges/release.svg" alt="release - release">
    </a>
    <a href="https://github.com/crgz/abbreviated_dates/actions?query=is%3Asuccess">
        <img src="https://github.com/crgz/abbreviated_dates/actions/workflows/04-ship.yml/badge.svg" alt="Status - Status">
    </a>
    <a href="https://github.com/crgz/abbreviated_dates/issues">
        <img src="https://img.shields.io/github/issues/crgz/abbreviated_dates.svg" alt="issues">
    </a>
    <a href="https://github.com/crgz/abbreviated_dates/stargazers">
        <img src="https://img.shields.io/github/stars/crgz/abbreviated_dates.svg" alt="stars - stars">
    </a>
    <a href="https://github.com/crgz/abbreviated_dates/graphs/contributors">
        <img src="https://img.shields.io/github/contributors/crgz/abbreviated_dates.svg" alt="stars - stars">
    </a>
    <a href="https://github.com/crgz/abbreviated_dates/blob/main/CONTRIBUTING.md">
        <img src="https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat" alt="contributions - contributions">
    </a>
</p>

<p align="center">
    <a href="#user-content-key-features">Key Features</a> •
    <a href="#user-content-how-to-use">How To Use</a> •
    <a href="#user-content-installation">Installation</a> •
    <a href="#user-content-how-it-works">How it works</a> •
    <a href="#user-content-common-use-cases">Common use cases</a> •
    <a href="#user-content-roadmap">Roadmap</a>
</p>

Have you ever tried to understand a date like *11-09, št*? Is the *št* an abbreviation of a month or a weekday? Which of
those numbers represent the month or the day? This library leverages on [Good Ol' Fashioned
AI](https://www.cambridge.org/core/books/abs/cambridge-handbook-of-artificial-intelligence/gofai/FCF7D6DD921658FE8AE9F2A2B0FECBDD)
to parse abbreviated, ambiguous, and incomplete dates in multiple languages.

## Key Features

* Python support through a [Python Bridge](https://github.com/crgz/fuzzy_dates)
* Language auto-detection
* Easily expandable into new languages (30 languages are currently supported)
* Support for multiple date formats
* Support for abbreviated weekdays
* Support for abbreviated months
* Support for ambiguous month/day numbers

## How To Use

The most straightforward way to parse dates is to use the abbreviated_dates:parse() predicate, that wraps around most of the
functionality of the module.  This example shows a basic usage of the library to parse the date: *"11-09, št"*:

```prolog
?- ['./prolog/demo.pl'], solutions('11-09, št').
╔═══════════════════════╤════════════╤════════════════╗
║         Date          │  Language  │    Country     ║
╟───────────────────────┼────────────┼────────────────╢
║ Saturday, 09 Nov 2024 │ Lithuanian │   Lithuania    ║
║ Saturday, 11 Sep 2027 │ Lithuanian │     Latvia     ║
║ Thursday, 11 Sep 2025 │   Slovak   │ Czech Republic ║
║ Thursday, 11 Sep 2025 │   Slovak   │    Slovakia    ║
║ Thursday, 11 Sep 2025 │   Slovak   │ Czech Republic ║
║ Thursday, 11 Sep 2025 │   Slovak   │    Slovakia    ║
╚═══════════════════════╧════════════╧════════════════╝
true.
```
<details>
  <summary>Click to see the demo code</summary>

```prolog
:- use_module(library(abbreviated_dates)).
:- use_module(library(cli_table)).

solutions(Text):- % E.g. solutions('11-09, št').
  Starting = date(2022,09,9),
  findall([Date,Language,Country],format(Starting,Text,Date,Language,Country),Row),
  cli_table(Row,[head(['Date','Language','Country'])]).

format(Starting, Text, DateText, Language, Country):-
  parse(Starting, Text, [Date], _, Language, Country),
  format_time(string(DateText), "%A, %d %b %Y", Date).
```
</details>

## Installation

To install required packages run:

```commandline
make install
```

To test the demo code shown above run this query in your SWI-Prolog shell:

```prolog
pack_install(cli_table).
```

## How it works

The abbreviation "št" could stand for:
- Šeštadienis which means in Saturday in Lithuanian
- Štvrtok which means in Thursday in Slovak

Lithuanian is spoken in Lithuania and in Latvia. Slovak is spoken in Slovakia but also by a minority in the Czech Republic.
These countries use different date representations: Czech Republic, Latvia and Slovakia have the day written first because of
the "little" date endianness format used as the standard in the country. Lithuania, on the other hand, uses the "big" date
endianness format which means that the month is written first. The system factor in all these facts and is able to come with
the right answers:

In the case of interpreting the abbreviation as a Saturday:
-  9 of November 2024
- 11 of September 2027

In the case of interpreting the abbreviation as a Thursday:
- 11 of September 2025

For further details have a look at the [implementation](prolog/abbreviated_dates.pl). In addition, the
[unit tests](prolog/abbreviated_dates.plt) might give an impression on how to use this library.

## Common use cases

Consuming data from different sources:

* Scraping: extract dates from different places with several formats and languages
* IoT: consuming data coming from different sources with different date formats
* Tooling: consuming dates from different logs / sources
* Format transformations: when transforming dates coming from different files (PDF, CSV, etc.) to other formats (database, etc).

## Roadmap

- [x] Multi-language Support
- [ ] Integrate with the Julian package

See the [open issues](https://github.com///issues) for a full list of proposed features (and known issues).

## Review

The package can be reviewed in the [Distribution Server](https://eu.swi-prolog.org/pack/review?p=abbreviated_dates)

## License

Distributed under the MIT License. See `LICENSE` file for more information.
