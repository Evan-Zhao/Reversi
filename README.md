# REVERSI
## Introduction
Reversi is a strategy board game for two players, played usually on an 8×8 board.
There are sixty-four identical game pieces called *disks*, which are light on one side and dark on the other. Players take turns placing disks on the board with their assigned color facing up.
During a play, any disks of the opponent's color that are in a straight line and bounded by *the disk just placed* and *another disk of the current player's color* are turned over to the current player's color.
The object of the game is to have the majority of disks turned to display your color when the last playable empty square is filled.

The rules adopted in this program is taken from [Wikipedia: Reversi](https://en.wikipedia.org/wiki/Reversi)

## Functionality
REVERSI is a command-line implementation of this game. Up  to now, the program implements
* Intuitive text UI and interaction
* Automated reversi board (8x8)
* A moderate-strength AI
AI of this game is based on the minmax search, a naive version.

## Todo
* Add user command such as -q, --help, --show
* (#) Experiments on simple ML for AI, for which TCP interface is required
