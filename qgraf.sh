#!/bin/sh
# This script improves the CLI of QGRAF by replacing its config
# file (qgraf.dat) with command line parameters. The config is
# then generated in a temporary directory, and QGRAF is
# executed there.
#
# Usage:
#   qgraf.sh \
#       --model=<modelfile> \
#       --style=<stylefile> \
#       --output=<outfile> \
#       --<qgraf option>=<value> ...
#
# The binary for QGRAF is taken to be just "qgraf", or $QGRAF,
# if that environment variable is defined.

tmpdir=$(mktemp -d)
trap "rm -rf '$tmpdir'" EXIT
cat >$tmpdir/qgraf.dat <<'EOF'
output='outputfile';
style='stylefile';
model='modelfile';
EOF

for arg in "$@"; do
    case $arg in
        --output=*) outputfile=${arg#*=};;
        --style=*) cp "${arg#*=}" "$tmpdir/stylefile";;
        --model=*) cp "${arg#*=}" "$tmpdir/modelfile";;
        --*=*)
            key=${arg%%=*};
            key=${key#--};
            val=${arg#*=};
            printf "%s=%s;\n" "$key" "$val" >> $tmpdir/qgraf.dat;
            ;;
        *)
            echo "unrecognized option: $arg";
            exit 1
            ;;
    esac
done
(cd "$tmpdir" && ${QGRAF:-qgraf}) || exit 1
[ -e "$tmpdir/outputfile" ] || exit 1
cp "$tmpdir/outputfile" "$outputfile" || exit 1
