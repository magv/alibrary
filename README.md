# Amplitude library

Pushing the boundaries of higher-loop amplitude calculations
in quantum field theory is no easy task, and no recipe works
for all. This means that one needs try different approaches,
investigate, and iterate. To this end, one can’t use a prepackaged
monolithic solution—one needs a library: *alibrary*.

*Alibrary* is a [Mathematica] library that provides functions to
generate Feynman diagrams, insert Feynman rules, sum over tensor
traces, construct IBP families, convert amplitudes to IBP notation,
etc, etc. It acts as a central hub to call into [QGRAF], [GraphViz],
[FORM] (with [color.h]), [Feynson], [LiteRed] and [FIRE], [Kira],
[pySecDec], etc, all while allowing you to develop interactively
(in a *read-eval-print loop* or an interactive notebook), inspect
and modify intermediate results, and generally progress towards
building a solution to your problem one step at a time.

[mathematica]: https://www.wolfram.com/mathematica/
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

Finally, *alibrary* aspires to be a learning resource, and have
all of its internals obvious and documented, so that one could
read and understand it without too much effort—this goal is
a work in progress; sorry.

## Documentation

The documentation currently lives at [magv.github.io/alibrary]
and consists of the following main annotated source files:

* [alibrary.m], the main library;
* [utils.m], the generic Mathematica helper functions;
* [atestsuite.m], the test suite for the above;
* [amodel-qcd.m], the QCD-like Feynman rules.
* [amodel-scalar.m], Feynman rules for a simple scalar theory.

And also the following auxiliary source files:

* [doc.py], the documentation generator;
* [kira.sh], the Kira command line wrapper;
* [library.m], older code that is to be cleaned up and merged into [alibrary.m].

Finally, the following files remain undocumented:

* color.h;
* all.tikzdefs;
* all.tikzstyles;
* library.frm;
* qgraf-stylefile;
* tempwrap.

The documentation is not really complete, but the plan is to
gradually improve it.

[magv.github.io/alibrary]: https://magv.github.io/alibrary/
[alibrary.m]: https://magv.github.io/alibrary/alibrary.html
[atestsuite.m]: https://magv.github.io/alibrary/atestsuite.html
[amodel-qcd.m]: https://magv.github.io/alibrary/amodel-qcd.html
[amodel-scalar.m]: https://magv.github.io/alibrary/amodel-scalar.html
[doc.py]: https://magv.github.io/alibrary/doc.html
[kira.sh]: https://magv.github.io/alibrary/kira.html
[library.m]: https://magv.github.io/alibrary/library.html
[mkdia.py]: https://magv.github.io/alibrary/mkdia.html
[utils.m]: https://magv.github.io/alibrary/utils.html

Additionally, the author gave a talk at [CAPP 2021] about
[Calculating loop amplitudes on a computer]: it describes how
to build a library like *alibrary*. The [code accompanying the
talk] is in essence a simplified version of this library, so
the presentation is an accurate description of the main ideas
and intended usage.

[capp 2021]: https://indico.desy.de/event/26814/
[calculating loop amplitudes on a computer]: https://indico.desy.de/event/26814/contributions/94605/attachments/65148/80414/magerya-capp2021.pdf
[code accompanying the talk]: https://indico.desy.de/event/26814/contributions/94605/attachments/65148/80413/magerya-capp2021.zip

### Example usage

As an example of the common usage, please take a look at:

* the calculation of QCD corrections to a [photon propagator];
* an explanation on [displaying diagrams].

[photon propagator]: https://magv.github.io/alibrary/examples/photon-propagator.html
[displaying diagrams]: https://magv.github.io/alibrary/examples/diagrams.html

## Installation

Download the source code, put it into a directory somewhere,
and import it into your Mathematica session like this:

    Get["/path/to/alibrary.m"];

Alternatively, to only import generic Mathematica functions
(those that are not related to loop amplitudes), use this:

    Get["/path/to/utils.m"];
