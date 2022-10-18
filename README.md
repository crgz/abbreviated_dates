<!-- PROJECT SHIELDS -->
[![Release!][release-shield]][release-url]
[![Status][submit-shield]][submit-url]
[![Issues][issues-shield]][issues-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Contributors][contributors-shield]][contributors-url]
[![MIT License][license-shield]][license-url]
[![contributions welcome][contributions-shield]][contributions-url]

<img src=".github/flags-jakearchibald.github.io-scour.svg?raw=true" width="25%" align="right" style="border:20px solid white">

# Parser for abbreviated, ambiguous, incomplete dates in multiple languages

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

Note: To install required packages run `make install` in your OS shell and `pack_install(cli_table).` in your SWI-Prolog shell

[<img alt="Sequence Diagram" width="30%" align="right"
src="https://crgz.github.io/abbreviated_dates/uml/png/sequence.png"/>](https://crgz.github.io/abbreviated_dates/uml/svg/sequence.svg)

## Eager to Contribute?

Please click on the Sequence Diagram and read the [CONTRIBUTING.md](./CONTRIBUTING.md) for further details regarding our GitHub Actions powered workflow:

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

## Review

The package can be reviewed in the [Distribution Server](https://eu.swi-prolog.org/pack/review?p=abbreviated_dates)

## Roadmap
- [x] Multi-language Support
- [x] Add back to top links

See the [open issues](https://github.com///issues) for a full list of proposed features (and known issues).

## Contributing! ❤️

Contributions are essential to keep our projects alive. I would like to keep it as easy as possible to contribute changes.
There are a few guidelines that I need contributors to follow so that all of us can benefit from quality control and quality
documentation. Please read the [CONTRIBUTING.md](./CONTRIBUTING.md) for further details

## License

Distributed under the MIT License. See `LICENSE` file for more information.

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[release-shield]: https://github.com/crgz/abbreviated_dates/blob/main/.github/badges/release.svg
[release-url]: https://www.swi-prolog.org/pack/list?p=abbreviated_dates
[submit-shield]: https://github.com/crgz/abbreviated_dates/actions/workflows/04-ship.yml/badge.svg
[submit-url]: https://github.com/crgz/abbreviated_dates/actions?query=is%3Asuccess
[issues-shield]: https://img.shields.io/github/issues/crgz/abbreviated_dates.svg
[issues-url]: https://github.com/crgz/abbreviated_dates/issues
[forks-shield]: https://img.shields.io/github/forks/crgz/abbreviated_dates.svg
[forks-url]: https://github.com/crgz/abbreviated_dates/network/members
[stars-shield]: https://img.shields.io/github/stars/crgz/abbreviated_dates.svg
[stars-url]: https://github.com/crgz/abbreviated_dates/stargazers
[contributors-shield]: https://img.shields.io/github/contributors/crgz/abbreviated_dates.svg
[contributors-url]: https://github.com/crgz/abbreviated_dates/graphs/contributors
[license-shield]: https://img.shields.io/github/license/crgz/abbreviated_dates.svg
[license-url]: https://github.com/crgz/abbreviated_dates/blob/main/LICENSE
[contributions-shield]: https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat
[contributions-url]: ./CONTRIBUTING.md

