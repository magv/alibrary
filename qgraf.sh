#!/bin/sh
# This script fixes the awful CLI of QGRAF by replacing its
# config file with command line parameters.
# Usage:
#   qgraf.sh \
#       --model=<modelfile> \
#       --style=<stylefile> \
#       --output=<outfile> \
#       --<key>=<value> ...

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
(cd "$tmpdir" && qgraf) || exit 1
[ -e "$tmpdir/outputfile" ] || exit 1
cp "$tmpdir/outputfile" "$outputfile" || exit 1
