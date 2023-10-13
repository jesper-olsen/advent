# Colosal Cave Adventure

Rusty port of Will Crowther and Don Woods' classic text adventure from 1977.
Wikipedia is a good source for the games history. Several slightly different versions of the game exist - including snapshots of the original Fortan code, Raymond's machine translated Fortran to C project and D. Knuth's hand translated Fortran to cweb implementation. Present version is based on all three, but mostly Knuth's.

See [walkthrough](WALKTHROUGH.txt) for example of how to make it to the end.

```
% cargo run --release
Welcome to Adventure!! Would you like instructions?
** n
OK.

You are standing at the end of a road before a small brick building. Around you is a forest. A small stream flows out of the building and down a gully.
*  
```

References:
* [Colosal Cave Adventure](https://en.wikipedia.org/wiki/Colossal_Cave_Adventure/). 
* [Adventure - CWEB port - Donald Knuth](https://www-cs-faculty.stanford.edu/~knuth/programs/advent.w.gz)
* [Open Adventure - C - Eric S Raymond](https://gitlab.com/esr/open-adventure/-/tree/master/)
* [Colosal Cave Adventure - Fortran - original source, collected by Carlos Aguilar](https://github.com/wh0am1-dev/adventure/tree/master/)
