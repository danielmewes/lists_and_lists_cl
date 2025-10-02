# Lists and Lists - Common Lisp Port

An interactive tutorial game for learning Scheme/Lisp programming.

## About

**Lists and Lists** is an educational interactive fiction game originally created by Andrew Plotkin in 1996 for the Z-machine (as part of the IF Competition). This is a faithful port to pure Common Lisp that runs natively without needing a Z-machine interpreter.

The game teaches Scheme/Lisp programming through 8 progressive tutorial problems, guided by a genie character.

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
./play.sh
```

## Saving and Loading

You can save your progress at any time using the `save` command, optionally providing a filename (defaults to `savegame.lisp`). Load your saved game later with the `load` command. Your current room, inventory, genie state, tutorial progress, and Scheme environment will all be preserved.

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

## License

This Common Lisp port follows the original's license: freely distributable and usable.

The Scheme interpreter code may be freely used, modified, and incorporated into other works.

## Credits

- **Original game**: Andrew Plotkin (1996)
- **Common Lisp port**: 2025
- **Beta testers** (original): Michael Kinyon, Dylan Thurston, Dave Seybert

Enjoy learning Lisp!
