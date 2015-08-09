# queens.hs

A Haskell program which displays solutions to the n-queens chess
problem.

On an n by n chess board, how can n queens be arranged so that
no queen is attacking any of the others?

I am still learning Haskell, and probably will be forever,
so this program is not meant to
be an example of good Haskell programming.

# Downloading queens.hs

Download the following files:

    queens.hs

    Board.hs

    Moves.hs

# Running queens.hs

The Glasgow Haskell Compiler (GHC) is required to compile this
program or to run it interpreted.  An easy way to get GHC
is to download the 
[Haskell Platform](https://www.haskell.org/platform/)

In the following, `n` is the size of the board.  The `options`
are explained below.

You can run the queens program without compiling as follows:

    runghc queens [ options ] n

Compilation results in higher speed, especially with `n` greater
than 12.  To compile:

    ghc -O2 queens.hs

(That's minus capital Oh 2, not minus zero 2)

Then run the resulting binary.

On Mac or Linux:

    ./queens [ options ] n

On Windows:

    queens.exe [ options ] n

Note:  I have tested this program only on my Mac.

# Options for queens.hs

    -h    help - displays usage and options.
    -d    display solutions as boards.  Can be either -d1 or -dall.
    -p    display solutions as lists of lists of pairs.  Each pair
          (x,y) is the location of a queen in the solution.  Can
          be either -p1 or -pall

# Examples

In all the examples, if you haven't compiled
the program, replace `./queens ...` with
`runghc queens.hs ...` .

Display one solution for an 8 by 8 board:

    ./queens 8

Display all solutions for an 8 by 8 board:

    ./queens -dall 8

Display one list of locations for an 8 by 8 board:

    ./queens -p1 8

Display all lists of locations for an 8 by 8 board:

    ./queens -pall 8

Try a different board size:

    ./queens -dall 4

    ./queens -dall 12

Get help:

    ./queens -h

# Output

By default the program will display one board with the first
solution it finds.  Using `-dall`, it will display all the
solutions up to one million boards.

With a large `n`, such as `n = 20`, you can adjust the size of
your command window to the size of one board display.  Then the
program appears to updating the screen in place, even though
the screen actually is scrolling through each board as it appears.

The output is created using CAG (Crappy Ascii Graphics).  Each
board is output using ascii characters, using Q's to
indicate where the queens are located.

Output of `./queens 8`:

    ---------------------------------
    | Q |   |   |   |   |   |   |   |
    ---------------------------------
    |   |   |   |   | Q |   |   |   |
    ---------------------------------
    |   |   |   |   |   |   |   | Q |
    ---------------------------------
    |   |   |   |   |   | Q |   |   |
    ---------------------------------
    |   |   | Q |   |   |   |   |   |
    ---------------------------------
    |   |   |   |   |   |   | Q |   |
    ---------------------------------
    |   | Q |   |   |   |   |   |   |
    ---------------------------------
    |   |   |   | Q |   |   |   |   |
    ---------------------------------

Output of `./queens -pall 4`:

    [(3,2),(2,0),(1,3),(0,1)]
    [(3,1),(2,3),(1,0),(0,2)]

# Caveats

I haven't tried a board size over 20.  With n = 20, the program will
never finish, because it stops after displaying one million boards.
With a board size of 20, there are 39,029,188,884 or almost 40 billion
solutions.  That's the
[American billion](https://en.wikipedia.org/wiki/Long_and_short_scales),
not the European or traditional British billion.
The one million board display limit is hard-coded into the prgram.

On my 17-inch, Late 2011, Macbook Pro, running the compiled version,
it took just over 2 hours and 18 minutes to display one million solutions
for n = 20.  I was running other stuff on the computer at the time.

WARNING:  My OS X Terminal app acted sluggish after I ran
`./queens -dall 20`.  It seemed to be okay after I quit and restarted
Terminal.

Some solutions are not unique if you consider rotation, reflection,
and combinations of rotation and reflection.  This program displays
all of them.
