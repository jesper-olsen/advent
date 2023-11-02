# Colossal Cave Adventure

Rusty port of Will Crowther and Don Woods' [classic text adventure](https://en.wikipedia.org/wiki/Colossal_Cave_Adventure/) from 1977. Mainly based on on D. Knuth's [cweb version](http://www.literateprogramming.com/adventure.pdf). Knuth's Advent is hand translated from Fortran and his cweb document is a delightful read with many little gems. Other versions I have looked at are Eric Raymond's Open Adventure - a much more "verbose" implementation - and some of the "original" Fortran sources collected by Carlos Aguilar. Aguilar's repo has a couple of walkthrough's which are useful, because some of the puzzles are not exactely natural.

See [walkthrough](WALKTHROUGH.txt) for example of how to make it to the end. Includes all the things you need to touch for scoring, but not all you can discover.

The game has 130 physical locations that can be visited. See the travel() function (cheat) for how they are connected - note that some transitions are probabilistic or depend on objects you carry or other characters in the game. Use shortest_route() function to calculate a route between two locations...

```
% cargo run --release
Welcome to Adventure!! Would you like instructions?
** n
OK.

You are standing at the end of a road before a small brick building. 
Around you is a forest. A small stream flows out of the building and 
down a gully.
*  
```

References:
* [Colossal Cave Adventure](https://en.wikipedia.org/wiki/Colossal_Cave_Adventure/) 
* [Chris Klimas - The History of Adventure](https://youtu.be/GrmWwzekMoY?si=ld4sW-M8JaRq1pVl)
* [Adventure - CWEB port - Donald Knuth](https://www-cs-faculty.stanford.edu/~knuth/programs/advent.w.gz)
* [Open Adventure - C - Eric S Raymond](https://gitlab.com/esr/open-adventure/-/tree/master/)
* [Colossal Cave Adventure - Fortran - original source, collected by Carlos Aguilar](https://github.com/wh0am1-dev/adventure/tree/master/)
