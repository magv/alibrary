# Amplitude library

Pushing the boundaries of higher-loop amplitude calculations
in quantum field theory is no easy task, and no recipe works for
all. This means that one needs try different approaches, investigate,
iterate. To this end, its useful to have a library: alibrary.

Alibrary provides functions to generate diagrams, insert Feynman
rules, sum over tensor traces, construct IBP families, convert
amplitudes to IBP notation, etc, etc. It acts as a central
hub to call into QGRAF, GraphViz, FORM, LiteRed & FIRE, Kira,
and pySecDec, all while allowing you to develop interactively
(in a read-evaluate-print loop, or an interactive notebook),
inspect and modify intermediate results, and progress towards
building the pipeline that solves your problem.

Alibrary tries to be light on abstraction. As much as possible
it works with data in the standard Mathematica notation, so that
the functions provided by it could be useful separately from the
rest of the library. In other words: it is a library, not a framework.
