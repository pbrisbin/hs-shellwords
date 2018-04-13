# ShellWords

Parse a string into words, like a shell would.

## Motivation

If you need to execute commands given to you as user-input, you should know not
to give that text as-is to a shell:

```haskell
callProcess "sh" ["-c", "some --user --input"]
```

Such code is a severe security vulnerability. Furthermore, any attempts to
sanitize the string are unlikely to be 100% affective and should be avoided. The
only safe way to do this is to not use a shell intermediary, and always `exec` a
process directly:

```haskell
callProcess "some" ["--user", "--input"]
```

The new problem (and not a security-related one) is how to correctly parse a
string like `"some --user --input"` into the command and its arguments. The
rules are complex enough that you probably want to get a library to do it.

So here we are.

## Example

```sh
Right (cmd:args) <- parse "some -complex --command=\"Line And\" 'More'

callProcess cmd args
--
-- Is equivalent to:
--
-- > callProcess "some" ["-complex", "--command=Line And", "More"]
--
```

## Lineage

This package is inspired by and named after

- [`python/ShellWords`][python-shellwords], which was itself inspired by
- [`go-shellwords`][go-shellwords], which was itself inspired by
- [`Parser::CommandLine`][parse-commandline]

[python-shellwords]: https://github.com/mozillazg/python-shellwords
[go-shellwords]: https://github.com/mattn/go-shellwords
[parse-commandline]: https://github.com/Songmu/p5-Parse-CommandLine

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
