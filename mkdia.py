#!/usr/bin/env python3

# # mkdia.py
#
# This is a wrapper over QGraf (actually [[qgraf.sh]]) to generate amplitudes.
#
# Usage:
#
#     mkdia.py [dia-]<incoming particles>-<outgoing particles>-<loop count>[.m]
#
# The list of particles is from `qgraf-modfile`. The style file is `qgraf-stylefile`.
#
# The momenta assignment: `q<i>` for incoming, `p<i>` for outgoing, `l<i>` for loop;
# with the momenta conservation given by `q=sum(q<i>)=sum(p<i>)`.

import sys
import re
import os

filename = "-".join(sys.argv[1:])
process = os.path.basename(filename)
m = re.match("(?:dia-)?([a-zA-Z]*)-([a-zA-Z]*)-([0-9]+)(?:.m)?", process)
if m is None:
    print("Bad process format:", repr(process))
    print("Usage: mkdia.py [dia-]<incoming particles>-<outgoing particles>-<loop count>[.m]")
    exit(1)

fieldsi, fieldso, loops = m.groups()

momi = ["q"] if len(fieldsi) == 1 else [f"q{i+1}" for i in range(len(fieldsi))]
momo = ["q"] if len(fieldso) == 1 else [f"p{i+1}" for i in range(len(fieldso))]

# Drop on-shell propagators ~ scaleless ints. Assuming on-shell
# legs if there is only one input and/or output.
massless = "qQgA"
opti = [f"--false=plink[{-1-2*i}]" for i, x in enumerate(fieldsi) if x in massless] if len(fieldsi) > 1 else []
opto = [f"--false=plink[{-2-2*i}]" for i, x in enumerate(fieldso) if x in massless] if len(fieldso) > 1 else []

base = os.path.dirname(sys.argv[0])
os.execv(f"{base}/qgraf.sh", [
    f"{base}/qgraf.sh",
    f"--output={filename}",
    f"--style={base}/qgraf-stylefile",
    f"--model={base}/qgraf-modfile",
    "--in={}".format(", ".join(f"{f}[{m}]" for f, m in zip(fieldsi, momi))),
    "--out={}".format(", ".join(f"{f}[{m}]" for f, m in zip(fieldso, momo))),
    f"--loops={loops}",
    "--loop_momentum=l",
    "--options=",
    *opti,
    *opto
])
