{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Case-for-case port from Python:
--
-- <https://github.com/mozillazg/python-shellwords/blob/master/tests/test_shellwords.py>
--
module ShellWordsSpec
    ( spec
    ) where

import Data.Foldable (for_)
import Data.Semigroup ((<>))
import ShellWords
import Test.Hspec

testCases :: [(String, [String])]
testCases =
    [ ("var --bar=baz", ["var", "--bar=baz"])
    , ("var --bar=\"baz\"", ["var", "--bar=baz"])
    , ("var \"--bar=baz\"", ["var", "--bar=baz"])
    , ("var \"--bar='baz'\"", ["var", "--bar='baz'"])
    , ("var --bar=`baz`", ["var", "--bar=`baz`"])
    , ("var \"--bar=\\\"baz'\"", ["var", "--bar=\"baz'"])
    , ("var \"--bar=\\'baz\\'\"", ["var", "--bar=\\'baz\\'"])
    , ("var \"--bar baz\"", ["var", "--bar baz"])
    , ("var --\"bar baz\"", ["var", "--bar baz"])
    , ("var --\"bar baz\"", ["var", "--bar baz"])

    -- Additional test cases for whitespace
    , ("var --bar=baz ", ["var", "--bar=baz"])
    , (" var --bar=baz ", ["var", "--bar=baz"])
    , (" var   --bar=baz ", ["var", "--bar=baz"])

    -- Additional test cases for escaped spaces
    , ("var --bar\\ baz", ["var", "--bar baz"])
    , ("var --bar=baz\\ bat", ["var", "--bar=baz bat"])
    , ("var --bar baz\\ bat", ["var", "--bar", "baz bat"])

    -- N.B. sh preserves escapes in quoted values, python-shellwords does not.
    -- we behave like sh in this regard. See omitted cases below too.
    , ("var --bar 'baz\\ bat'", ["var", "--bar", "baz\\ bat"])
    , ("var  --bar \"baz\\ bat\"", ["var", "--bar", "baz\\ bat"])
    ]

    -- Omitted cases:
    --
    -- I think the Python test case is wrong here:
    --
    -- > sh-4.4$ shellwords() { printf ">%s<\n" "$@"; }
    -- > sh-4.4$ shellwords var "--bar=\'baz\'"
    -- > >var<
    -- > >--bar=\'baz\'<
    --
    -- , ("var \"--bar=\\'baz\\'\"", ["var", "--bar='baz'"])
    --
    --
    -- I can't get this one to work:
    --
    -- , ("var --bar='\\'", ["var", "--bar=\\"])

errorCases :: [String]
errorCases =
    [ "foo '"
    , "foo \""

    --
    -- Doesn't need to error since we don't have parse_backtick either.
    --
    -- , "foo `"
    ]

spec :: Spec
spec = describe "parse" $ do
    for_ testCases $ \(input, expected) -> do
        it ("parses |" <> input <> "| correctly") $ do
            parse input `shouldBe` Right expected

    for_ errorCases $ \input -> do
        it ("errors on |" <> input <> "|") $ do
            parse input `shouldSatisfy` isLeft

    it "fixes #3" $ do
        let input = "-LC:/Users/Vitor\\ Coimbra/AppData/Local/Programs/stack/x86_64-windows/msys2-20150512/mingw64/lib -ltag\n"
            expected =
                [ "-LC:/Users/Vitor Coimbra/AppData/Local/Programs/stack/x86_64-windows/msys2-20150512/mingw64/lib"
                , "-ltag"
                ]

        parse input `shouldBe` Right expected

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

{- Features I'm not sure I'll be porting, certainly not yet.

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
