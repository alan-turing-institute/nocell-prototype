# nocell (prototype)

[![Build Status](https://travis-ci.com/alan-turing-institute/nocell.svg?token=ZPDxx69KHKrd5gefprNs&branch=develop)](https://travis-ci.com/alan-turing-institute/nocell-prototype)

**Note: Current development of nocell is in https://github.com/alan-turing-institute/nocell.**

**This repository is no longer actively maintained**

A language for building probabilistic spreadsheets

Development is on the develop branch. Design thoughts are in the wiki.

## Dependencies

* Racket
* zip
* diff (testing only)

## How we work together

Branches

* Master - is demo-able
* Develop - the tests pass
* Feature branches
* Pull requests - If you want a review, ALWAYS if you change a defines, as you see fit

Style 

* main.rkt in each subdirectory
* Explcitly provide rather than all defined out
* Use contract-out on provides
* No owners, but let people know what you are working on (slack, grabbing an issue)
