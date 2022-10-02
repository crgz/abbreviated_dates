<!-- PROJECT SHIELDS -->
[![Release!][release-shield]][release-url]
[![Status][submit-shield]][submit-url]
[![Issues][issues-shield]][issues-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Contributors][contributors-shield]][contributors-url]
[![MIT License][license-shield]][license-url]
[![contributions welcome][contributions-shield]][contributions-url]
[![Ask Me Anything!][ask-shield]][ask-url]

<img src=".github/flags-jakearchibald.github.io-scour.svg?raw=true" width="40%" align="right" style="border:20px solid white">

# Abbreviated Date Parser

The Abbreviated Date Parser Library contains a predicate that parses abbreviated incomplete dates in multiple languages.

## Getting Started
Imagine you want to understand the date: "11-09, št". We can infer that "št" is an abbreviation of:

```prolog
findall([F,L,C],(parse(date(2022,09,9),'11-09, št',[D],_,L,C),format_time(string(F),"%A, %d %b %Y",D)),Y),cli_table(Y,[head(['Date','Language','Country'])]).
```

```
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
```
- Šeštadienis which means in Saturday in Lithuanian
- Štvrtok which means in Thursday in Slovak

But Lithuanian is spoken not only in Lithuania but also in Latvia and Slovak is spoken not only in Slovakia but also by a minority in Czech Republic. These countries use different date representations: Czech Republic, Latvia and Slovakia have the day written first because of the "little" date endianness format used as the standard in the country. Lithuania, on the other hand, uses the "big" date endiannes format which means that the month is written first.

The system factor in all this facts and is able to come with the right answers:

In the case of interpreting the abbreviation as s Saturday:
-  9 of November 2024
- 11 of September 2027

In the case of interpreting the abbreviation as a Thursday:
- 11 of September 2025

## Getting Started

This pack is available from the [add-on registry of SWI-Prolog](http://www.swi-prolog.org/pack/list).

It can be installed with `pack_install/1`:

```prolog
?- pack_install(abbreviated_dates).
```

Then, you can use it by simply calling `use_module(library(abbreviated_dates))`.

```prolog 
parse(Context, Expression, Dates, Trace).
```

### Prerequisites

For maintenance tasks [bumpversion](https://github.com/peritus/bumpversion) and [hub](https://github.com/github/hub)
might be required.

### Installation

## Usage

```prolog
parse/4.  % parse an abbreviated incomplete date in multiple languages (today, tomorrow, etc).
```
For further details have a look at the [implementation](prolog/abbreviated_dates.pl). In addition, the
[new](prolog/abbreviated_dates.plt) might give an impression on how to use this library.

The tests can be run using the following command:

```shell
make test
```
Define new Tests

New tests should be defined in the [test](prolog/abbreviated_dates.plt) file.

Review: 
https://eu.swi-prolog.org/pack/review?p=abbreviated_dates

## Roadmap
- [x] Multi-language Support
- [x] Add back to top links

See the [open issues](https://github.com///issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions
you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply
open an issue with the tag "enhancement". Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## License

Distributed under the MIT License. See `LICENSE` file for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[release-shield]: https://github.com/crgz/abbreviated_dates/blob/main/.github/badges/release.svg
[release-url]: https://www.swi-prolog.org/pack/list?p=abbreviated_dates
[submit-shield]: https://github.com/crgz/abbreviated_dates/actions/workflows/submit.yml/badge.svg
[submit-url]: https://github.com/crgz/abbreviated_dates/actions/workflows/submit.yml
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
[contributions-url]: https://github.com/crgz/abbreviated_dates/issues
[ask-shield]: https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg
[ask-url]: https://github.com/crgz
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/crgz
