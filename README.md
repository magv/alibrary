# Amplitude library

Pushing the boundaries of higher-loop amplitude calculations
in quantum field theory is no easy task, and no recipe works
for all. This means that one needs try different approaches,
investigate, and iterate. To this end, one can’t use a prepackaged
monolithic solution—one needs a library: *alibrary*.

*Alibrary* is a Mathematica library that provides functions to
generate Feynman diagrams, insert Feynman rules, sum over tensor
traces, construct IBP families, convert amplitudes to IBP notation,
etc, etc. It acts as a central hub to call into [QGRAF], [GraphViz],
[FORM] (with [color.h]), [Feynson], [LiteRed] and [FIRE], [Kira],
[pySecDec], etc, all while allowing you to develop interactively
(in a *read-eval-print loop* or an interactive notebook), inspect
and modify intermediate results, and generally progress towards
building a solution to your problem one step at a time.

[qgraf]: http://cfif.ist.utl.pt/~paulo/qgraf.html
[graphviz]: https://graphviz.org/
[form]: https://www.nikhef.nl/~form/
[color.h]: https://www.nikhef.nl/~form/maindir/packages/color/
[feynson]: https://github.com/magv/feynson
[litered]: https://www.inp.nsk.su/~lee/programs/LiteRed/
[fire]: https://bitbucket.org/feynmanIntegrals/fire/
[kira]: https://gitlab.com/kira-pyred/kira
[pysecdec]: https://github.com/gudrunhe/secdec

*Alibrary* tries to be light on abstraction. As much as possible
it works with data in the standard Mathematica notation, so that
each of its functions could be useful separately from the rest,
and its data could be operated on using normal Mathematica
commands. In other words: *alibrary* is a library, not a framework.

*Alibrary* is meant to be extended, modified, or simply learned
from. Ultimately *you* are doing the calculation, *alibrary*
is just one of the pieces of code you might be using. Its goal
is not to be a big deal.

Finally, *Alibrary* is a learning resource. Its source is (or,
will be) documented, and you are expected to be able to read
and understand it.

## Installation

Download the source code, put it into a directory somewhere,
and import it into your Mathematica session like this:

    Get["/path/to/alibrary.m"];
