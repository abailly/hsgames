# Acquire in Haskell

This is a rough implementation of [Acquire](http://www.webnoir.com/bob/sid/acquire.htm) boardgame as a client-server online game in
Haskell. It's an early-stage implementation but is already playable, even in solitaire mode against randomly playing robots.

## Overview

The game is played in client-server mode:

* The server can sport several games concurrently,
* Client players can create a new game or list existing games,
* Client players can join an existing non-started game.

Server uses TCP over port 7890 by default for all operations.

## Getting Started

To play in solo mode against robots, run the following commands in the source tree, assuming you are using
[stack](https://github.com/commercialhaskell/stack) to build code (works equally well with cabal with more incantations):

### To Start server

    $ stack init
    $ stack build
    $ <PATH TO BIN>/acq server

### To Create Game

Open another terminal and run:

    $ <PATH TO BIN>/acq newGame -H 1 -R 5
    created new game DBEGXBIZ

This creates a new game with one human and five robot players.

You can check game is available by running:

    $ <PATH TO BIN>/acq list
    DBEGXBIZ 3 humans, 3 robots, []

### To Start Playing

Connect as a player:

    $ <PATH TO BIN>/acq player -g DBEGXBIZ -n arnaud
    registering arnaud with server at address 127.0.0.1:7890
    registered player arnaud with game DBEGXBIZ
    starting game DBEGXBIZ

You should see something like the following:

    A-1  A-2  A-3  A-4  A-5  A-6  A-7  A-8  A-9  A-10 A-11 A-12
    B-1  B-2  B-3  B-4  B-5  B-6  B-7  B-8  B-9  B-10 B-11 B-12
    C-1  C-2  C-3  C-4  C-5  C-6  C-7  C-8  C-9  C-10 C-11 C-12
    D-1  D-2  D-3  D-4  D-5  D-6  D-7  D-8  D-9  D-10 D-11 D-12
    E-1  E-2  E-3  E-4  E-5  E-6  E-7  E-8  E-9  E-10 E-11 E-12
    F-1  F-2  F-3  F-4  F-5  F-6  F-7  F-8  F-9  F-10 F-11 F-12
    G-1  G-2  G-3  G-4  G-5  G-6  G-7  G-8  G-9  G-10 G-11 G-12
    H-1  H-2  H-3  H-4  H-5  H-6  H-7  H-8  H-9  H-10 H-11 H-12
    I-1  I-2  I-3  I-4  I-5  I-6  I-7  I-8  I-9  I-10 I-11 I-12
    arnaud [H] [I-3 
               ,B-2 
               ,I-2 
               ,A-4 
               ,F-4 
               ,H-9 ]
               [] $6000
    Your move, arnaud ?
    1- Place "arnaud" (Tile {tileCoords = ('I',3)})
    2- Place "arnaud" (Tile {tileCoords = ('B',2)})
    3- Place "arnaud" (Tile {tileCoords = ('I',2)})
    4- Place "arnaud" (Tile {tileCoords = ('A',4)})
    5- Place "arnaud" (Tile {tileCoords = ('F',4)})
    6- Place "arnaud" (Tile {tileCoords = ('H',9)})

Then select your move by typing the corresponding number.

## Some details

* Server runs by default on port 7890, set the port using `-p <some port>` number
* Server outputs all order in its console in order to allow debugging and tracing of sequence of plays
* Server saves game state between each order in a file `.acquire.bak` in working directory.
* If a saved game exists in working directory, server loads it and continues game from the saved point
* Client expects a valid input, it will crash otherwise.
