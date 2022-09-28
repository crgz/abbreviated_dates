<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a name="readme-top"></a>

<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Best-README-Template</h3>

  <p align="center">
    An awesome README template to jumpstart your projects!
    <br />
    <a href="https://github.com/othneildrew/Best-README-Template"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/othneildrew/Best-README-Template">View Demo</a>
    ·
    <a href="https://github.com/othneildrew/Best-README-Template/issues">Report Bug</a>
    ·
    <a href="https://github.com/othneildrew/Best-README-Template/issues">Request Feature</a>
  </p>
</div>

[![Submit](https://github.com/crgz/abbreviated_dates/actions/workflows/submit.yml/badge.svg)](https://github.com/crgz/abbreviated_dates/actions/workflows/submit.yml)

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

The tests can be run using the following command:

```shell
make test
```

### Define new Tests

New tests should be defined in the [test](prolog/abbreviated_dates.plt) file.

## Review
https://eu.swi-prolog.org/pack/review?p=abbreviated_dates
