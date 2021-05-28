#!/bin/sh

# # kira.sh
#
# This script is a wrapper around the `kira` executable that
# first copies the jobs file (along with its whole directory)
# into a temporary location, executes Kira there, and copies back
# the IBP tables (`result/*/*.m`) and `kira.log`. If Kira
# finishes successfully, this script will create a file called
# `done` next to the jobs file.
#
# The Kira binary being executed is `$KIRA` if that environment
# variable exists, otherwise just `kira`.

if [ $# < 1 ]; then
    echo "usage: $0 /path/to/jobs.yaml [kira-arguments] ..." 1>&2
    exit 1
fi

yaml="$1"
shift 1

yamldir=$(dirname "$yaml")
yamlbase=$(basename "$yaml")

if [ ! -e "$yaml" -o ! -d "$yamldir" ]; then
    echo "$0: no such file as $yaml" 1>&2
    exit 1
fi

tmpdir="$(mktemp -d)"
cp -va "$yamldir/." "$tmpdir/" || exit 1
(cd "$tmpdir" && ${KIRA:-kira} "$yamlbase" "$@")
code="$?"
if [ -e "$tmpdir/results" ]; then
    rm -f "$tmpdir/results/kira.db"
    cp -a "$tmpdir/results" "$yamldir/"
fi
if [ -e "$tmpdir/kira.log" ]; then
    cp -a "$tmpdir/kira.log" "$yamldir/"
fi
rm -rf "$tmpdir"
if [ "$code" -eq 0 ]; then
    date > "$yamldir/done"
else
    echo "Error: ${KIRA:-kira} failed with exit code $code"
fi
exit "$code"
