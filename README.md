# Basic Theorem Prover, under development. #

## Current features ##

See MathTest.hs for a demonstration, and to see how the tactical language works. (Currently the only tactics are "forwardReason2", "exportProp2", and "unfoldProp2".)

Currently, you can use it to manually prove things in propositional calculus (MathTest.hs has the 3 minimal rules you need; you can provide your own).

For graph visualization: Copy the output dot code to <file>.dot. Install graphviz and run 

dot -Tpdf <file>.dot > <file>.pdf.

## Design philosophy ##

The eventual goal is to produce a human-style theorem prover. It will have the following features.

* It will keep a complete proof script in memory, and hence be able to "articulate" what it is doing, as in [SHRDLU](http://en.wikipedia.org/wiki/SHRDLU). Messages come from the application of tactics, which are arranged in a hierarchy.
* It stores the propositions in a (nested) directed graph format (rather than just keeping a list of assumptions/goals) and is able to display the graph.
* It will have programmable tacticals - some basic tactics and a rich set of combinators (all in Haskell, so no separate language required).
* It will offer mathematician-friendly syntax.

## TODO ##

* Add a basic type system.
* Implement substitution as a tactic.
* Implement backwards reasoning.
* Add "pretty-printing": combine with LaTeX as in http://alessandrovermeulen.me/2013/07/08/combining-graphviz-dot-and-tikz-with-dot2tex/.
* Add basic human-type strategies. See for instance, http://arxiv.org/abs/1309.4501.

