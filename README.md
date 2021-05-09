# rescript-mode

An Emacs major mode for [ReScript](https://rescript-lang.org/).

This is based off of
[reason-mode](https://github.com/reasonml-editor/reason-mode).
Support for ReasonML has been removed.  This repo will only target
ReScript v9 and be tested against Emacs 27.

## Manual Installation

Install like any other Emacs package:

```lisp
(add-to-list 'load-path "/path/to/this/repo")
(require 'rescript-mode)
```

## Tests via Cask + ERT

The `test` folder contains tests that can be run via
[Cask](https://github.com/cask/cask).  Once you install `cask`, if it
is the first time run:

```
cask install
cask exec ./run_tests.sh
```

If it is not the first time you can omit the first line and execute
the tests with the second one only.  The environment variable EMACS
controls the program that runs emacs.

## License

`rescript-mode` is distributed under the terms of both the MIT license
and the Apache License (Version 2.0).

See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE)
for details.
