edan40-chatterbot
=================

A chatterbot implementation in Haskell.


Funktionen match wildcard p s försöker att matcha de två listorna p (p:ps) och s (s:ss). Listan p anses vara ett mönster som innehållet element lika med wildcard. Listan s får inte innehålla några wildcards. Mönstret p matchar listan s om varje element i p matchar motsvarande element i s. 

Ett non-wildcard matchas ett enstaka element i s om de är lika (p == s), och ett wildcard matchar en arbiträr lång sublista. 

Om de två listorna matchas, returnerar funktionen Just r, där r är sublistan i s som matchar den första förekomsten av wildcard i p.

Om listorna inte matchar, är resultatet Nothing.

