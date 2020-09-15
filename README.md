# nocell (prototype)

[![Build Status](https://travis-ci.com/alan-turing-institute/nocell.svg?token=ZPDxx69KHKrd5gefprNs&branch=develop)](https://travis-ci.com/alan-turing-institute/nocell-prototype)

**Note: This repository is no longer actively maintained.  Current development of nocell is in https://github.com/alan-turing-institute/nocell.**

A language for building probabilistic spreadsheets

## Dependencies

* [Racket](https://racket-lang.org)

## Installation and getting started

To install the package, run `raco pkg install` in this directory.

After installation, running `raco doc nocell` will open the nocell documentation in your web browser.

The [discounted cash flow calculator](https://github.com/alan-turing-institute/discounted-cash-flow-calculator) is an extended example written in nocell.

You might also like to browse the examples in this repository: [examples](examples) and [test-examples](test/test-examples).

Design thoughts are in the [wiki](https://github.com/alan-turing-institute/nocell-prototype/wiki).

## How we work together

Branches

* `master` - is demo-able
* `develop` - the tests pass
* `feature/xy` - Feature branches
* Pull requests - Mandatory, if you change an interface; Optional, if you want a review.

Style 

* main.rkt in each subdirectory
* Explcitly provide rather than all defined out
* Use contract-out on provides
* No owners, but let people know what you are working on (slack, grabbing an issue)
