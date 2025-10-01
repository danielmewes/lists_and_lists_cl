# Lists and Lists - Common Lisp Port

An interactive tutorial game for learning Scheme/Lisp programming.

## About

**Lists and Lists** is an educational interactive fiction game originally created by Andrew Plotkin in 1996 for the Z-machine (as part of the IF Competition). This is a faithful port to pure Common Lisp that runs natively without needing a Z-machine interpreter.

The game teaches Scheme/Lisp programming through 8 progressive tutorial problems, guided by a genie character. Players work through problems involving:

1. Basic definitions
2. List manipulation (CAR, CDR, CONS)
3. Functions and conditionals
4. Recursion
5. Advanced recursion
6. Higher-order functions
7. Closures and static scoping

## Features

- **Complete Scheme interpreter** implemented in Common Lisp
  - Supports: define, lambda, quote, if, cond, let, letrec
  - Built-in functions: +, -, *, car, cdr, cons, equal?, eqv?, etc.
  - Proper static scoping and closures
  - Infinite loop protection

- **Interactive game narrative** with rooms, objects, and a helpful genie guide

- **8 tutorial problems** with automatic checking and hints

- **Manual system** (simplified from original)

- **Pure Common Lisp** - no external dependencies, runs on any CL implementation

## Requirements

- A Common Lisp implementation (SBCL, CCL, CLISP, etc.)
- No external libraries required

## Running the Game

### From SBCL REPL:
```lisp
(load "lists-and-lists.lisp")
(lists-and-lists:play-game)
```

### From the command line:
```bash
sbcl --load lists-and-lists.lisp --eval "(lists-and-lists:play-game)"
```

### Running tests:
```bash
sbcl --script test-game.lisp
```

## How to Play

1. Start the game and go north through the door
2. Break the glass box to wake the genie
3. Answer "yes" to start the tutorial
4. Push the green button (or type "run") to use the Scheme interpreter
5. Solve the problems and type "check" when ready
6. Type "help" or "hint" if you get stuck

## Game Commands

- **look** - Examine your surroundings
- **north/south** - Move between rooms
- **examine [object]** - Look at something
- **break [object]** - Break the glass box
- **run** (or **push green**) - Start the Scheme interpreter
- **reset** (or **push yellow**) - Reset the interpreter
- **yes/no** - Answer the genie
- **check** - Have the genie check your solution
- **repeat** - Repeat the current problem
- **help/hint** - Get hints
- **about** - About the game
- **manual** - View the manual
- **quit** - Exit the game

## Scheme Interpreter Commands

When in the interpreter (after typing "run"):

- **:q** - Exit the interpreter
- **:?** - Show help
- **:m** - View the manual
- **:r** - Repeat the current problem
- **:c** - Cancel current input
- **:e** - Show environment (not implemented)

## Example Session

```
> north
> examine box
> break box
> yes
> run

>> (define twentyseven 27)
 27
>> :q

> check
"Aha! Very good."
```

## Differences from Original

This port maintains the core gameplay and educational content while adapting to Common Lisp:

- **Native Common Lisp**: Runs directly, no Z-machine needed
- **Simplified interface**: Text-based commands instead of parser
- **Abbreviated manual**: Core concepts provided, full manual in original
- **Same problems**: All 8 tutorial problems faithfully reproduced
- **Same Scheme interpreter**: Equivalent functionality and behavior

## Original Game

**Lists and Lists** was created by Andrew Plotkin (erkyrath@netcom.com) for the 1996 Interactive Fiction Competition. The original Z-machine source code is freely distributable and usable.

Original copyright: Copyright 1996 by Andrew Plotkin

## Files

- `lists-and-lists.lisp` - Main game implementation
- `test-game.lisp` - Test script
- `README.md` - This file
- `original/` - Original Z-machine source files
  - `readme` - Original readme
  - `lists.inf` - Game source
  - `zlisp-core.inf` - Scheme interpreter core
  - `zlisp-funs.inf` - Scheme interpreter functions
  - `lists-manual.inf` - Full manual
  - `zlisp.inf` - Simple execution shell

## License

This Common Lisp port follows the original's license: freely distributable and usable.

The Scheme interpreter code may be freely used, modified, and incorporated into other works.

## Credits

- **Original game**: Andrew Plotkin (1996)
- **Common Lisp port**: 2025
- **Beta testers** (original): Michael Kinyon, Dylan Thurston, Dave Seybert

Enjoy learning Lisp!
