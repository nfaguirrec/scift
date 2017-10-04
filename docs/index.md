---
layout: default
title: SciFT (Scientific Fortran Tools)
mathjax: true
---

<div id="news">
  <h2>News</h2>
  <ul class="posts">
    {% for post in site.posts %}
      <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></li>
    {% endfor %}
  </ul>
</div>

## SciFT (Scientific Fortran Tools)

The Scientific Fortran Tools (SciFT) is a numerical library for fortran programmers.
The library provides a wide range of mathematical routines such as random number generators,
special functions and least-squares fitting.

## Table of contents

- [Quick start](#quick-start)
- [Bugs and feature requests](#bugs-and-feature-requests)
- [Documentation](#documentation)
- [Authors](#authors)
- [Copyright and license](#copyright-and-license)

The complete range of subject areas covered by the library includes:

<div id="sections">
  <ul class="page">
    {% for page in site.pages %}
        {% if page.class == "section" %}
            <li> <a href="{{ site.baseurl }}{{ page.url }}">{{ page.title }}{{ page.relative_directory }}</a></li>
        {% endif %}
    {% endfor %}
  </ul>
</div>


## Quick start

Several quick start options are available:

- [Download the latest release.](https://github.com/twbs/bootstrap/archive/v4.0.0-beta.zip)
- Clone the repo: `git clone https://github.com/nfaguirrec/scift`

<!-- Read the [Getting started page](https://getbootstrap.com/getting-started/) for information on the framework contents, templates and examples, and more. -->


## Bugs and feature requests

Have a bug or a feature request? Please first read the [issue guidelines](https://github.com/twbs/bootstrap/blob/master/CONTRIBUTING.md#using-the-issue-tracker) and search for existing and closed issues. If your problem or idea is not addressed yet, [please open a new issue](https://github.com/twbs/bootstrap/issues/new).


## Documentation

Bootstrap's documentation, included in this repo in the root directory, is built with [Jekyll](https://jekyllrb.com) and publicly hosted on GitHub Pages at <https://getbootstrap.com>. The docs may also be run locally.


## Authors

**Nestor F. Aguirre**
- <https://github.com/nfaguirrec>

## Copyright and license

Code and documentation copyright 2010-2017 by Nestor F.Aguirre. Code and documentation released under the [MIT License](https://github.com/nfaguirrec/scift/blob/master/LICENSE).
