module ShellWordsSpec
    ( spec
    ) where

import ShellWords
import Test.Hspec

spec :: Spec
spec = undefined

{-
#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function, unicode_literals
import os

from shellwords import ShellWords, shell_run

test_cases = {
    'var --bar=baz': ['var', '--bar=baz'],
    'var --bar="baz"': ['var', '--bar=baz'],
    'var "--bar=baz"': ['var', '--bar=baz'],
    '''var "--bar='baz'"''': ['var', "--bar='baz'"],
    "var --bar=`baz`": ['var', "--bar=`baz`"],
    r'''var "--bar=\"baz'"''': ['var', """--bar="baz'"""],
    r'''var "--bar=\'baz\'"''': ['var', "--bar='baz'"],
    r"var --bar='\'": ['var', "--bar=\\"],
    'var "--bar baz"': ['var', '--bar baz'],
    'var --"bar baz"': ['var', '--bar baz'],
    'var  --"bar baz"': ['var', '--bar baz'],
}


def test_simple():
    s = ShellWords()
    for (line, expected) in test_cases.items():
        print(repr(line))
        args = s.parse(line)
        assert args == expected


def test_error():
    s = ShellWords()
    for x in ["foo '", 'foo "', "foo `"]:
        try:
            s.parse(x)
        except:
            pass
        else:
            raise Exception("Should be an error")


def test_backtick():
    goversion, err = shell_run("go version")
    assert not err

    s = ShellWords(parse_backtick=True)
    args = s.parse("echo `go version`")

    expected = ["echo", goversion.strip('\n')]
    assert args == expected


def test_backtick_error():
    s = ShellWords(parse_backtick=True)
    try:
        s.parse("echo `go Version`")
    except:
        pass
    else:
        raise Exception("Should be an error")


def test_env():
    os.environ["FOO"] = "bar"

    s = ShellWords(parse_env=True)
    args = s.parse("echo $FOO")

    expected = ["echo", "bar"]
    assert args == expected


def test_no_env():
    s = ShellWords(parse_env=True)
    args = s.parse("echo $BAR")

    expected = ["echo", ""]
    assert args == expected


def test_dup_env():
    os.environ["FOO"] = "bar"
    os.environ["FOO_BAR"] = "baz"

    s = ShellWords(parse_env=True)
    args = s.parse("echo $$FOO$")
    expected = ["echo", "$bar$"]
    assert args == expected

    args = s.parse("echo $${FOO_BAR}$")
    expected = ["echo", "$baz$"]
    assert args == expected
-}
