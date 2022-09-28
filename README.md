<!-- PROJECT SHIELDS -->
[![Submit][submit-shield]][submit-url]
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

<img src=".github/flags-jakearchibald.github.io-scour.svg?raw=true" width="50%" align="right" style="border:20px solid white">

# Abbreviated Date Parser

The Abbreviated Date Parser Library contains a predicate that parses abbreviated incomplete dates in multiple languages.

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>

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
- [ ] 

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

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Contact

Your Name - [@your_twitter](https://twitter.com/your_username) - email@example.com

Project Link: [https://github.com/your_username/repo_name](https://github.com/your_username/repo_name)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Acknowledgments

* [Img Shields](https://shields.io)
* [GitHub Pages](https://pages.github.com)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[submit-shield]: https://github.com/crgz/abbreviated_dates/actions/workflows/submit.yml/badge.svg
[submit-url]: https://github.com/crgz/abbreviated_dates/actions/workflows/submit.yml
[contributors-shield]: https://img.shields.io/github/contributors/crgz/abbreviated_dates.svg
[contributors-url]: https://github.com/crgz/abbreviated_dates/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/crgz/abbreviated_dates.svg
[forks-url]: https://github.com/crgz/abbreviated_dates/network/members
[stars-shield]: https://img.shields.io/github/stars/crgz/abbreviated_dates.svg
[stars-url]: https://github.com/crgz/abbreviated_dates/stargazers
[issues-shield]: https://img.shields.io/github/issues/crgz/abbreviated_dates.svg
[issues-url]: https://github.com/crgz/abbreviated_dates/issues
[license-shield]: https://img.shields.io/github/license/crgz/abbreviated_dates.svg
[license-url]: https://github.com/crgz/abbreviated_dates/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/crgz
