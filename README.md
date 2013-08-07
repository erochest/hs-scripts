
This is a collection of small utilities and shell-programs written in
Haskell. Originally this was to give me more of a chance to exercise my
Haskell chops, but it's turned out to be a pretty good language for this too.

Here are the utilities.

# `count-items`

This takes a sorted list of input and returns the number of times each unique
item is found in the input. The output is a TSV listing the input line and
the output.

# `dedup`

This walks the current directory and gets a SHA1 hash for every file. Any
files with the same SHA1 are printed out. The output is a TSV listing of the
duplicates. The first column is the SHA1 and the second is a filename with
that hash. Only duplicate files are output.

# `new-hs`

This creates a new Haskell project directory, setting up cabal, git, and
publishing it to Github. See the file itself for more details.

# `slice`

This returns a slice of the lines in a file. Either the starting position or
the ending one are optional. The input is read from STDIN, and the slice is
written to STDOUT.

```bash
slice FROM-TO
```

# `sumint`

This takes a list of integers for input and outputs their sum.

