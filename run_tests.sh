#!/usr/bin/env bash
# Copyright 2014 The Rust Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution and at
# http://rust-lang.org/COPYRIGHT.
# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.
#
# This runs the test for emacs rescript-mode.
# Either $EMACS must be set, or it must be possible to find emacs via PATH.

if [ -z "$EMACS" ]; then
    EMACS=emacs
fi

$EMACS --batch || {
    echo "You must set EMACS to a program that runs emacs."
    exit 1
}

$( $EMACS -batch > /dev/null 2>&1 ) || {
    echo "Your emacs command ($EMACS) does not run properly."
    exit 2
};

$( $EMACS -batch --eval "(require 'ert)" > /dev/null 2>&1 ) || {
    echo 'You must install the `ert` dependency; see README.md'
    exit 3
};

# All the files rescript-mode depends on (in dependency order!)
DEPS_INCLUDES="-l rescript-indent.el -l rescript-interaction.el"

rm *.elc
WARNINGS="$($EMACS -Q -batch $DEPS_INCLUDES -f batch-byte-compile rescript-mode.el 2>&1 | grep -v '^Wrote ')"
if [ -n "$WARNINGS" ]; then
    echo "Byte-compilation failed:"
    # echo "$WARNINGS"
    exit 4
else
    rm *.elc
    echo "Byte-compilation passed."
fi

TEST_INCLUDES=$(ls test/*.el | sed -e 's/^/-l /' | xargs)

if [ -n "$1" ]; then
    export ERT_SELECTOR="$1"
    $EMACS -batch -l ert $DEPS_INCLUDES -l rescript-mode.el $TEST_INCLUDES \
           -eval '(ert-run-tests-batch-and-exit (getenv "ERT_SELECTOR"))'
else
    $EMACS -batch -l ert $DEPS_INCLUDES -l rescript-mode.el $TEST_INCLUDES \
           -f ert-run-tests-batch-and-exit
fi
