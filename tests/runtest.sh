#!/bin/sh
#
# $Id$

GETDESC="xsltproc --novalid description.xsl"
AWK=awk
URPAL=./urpal
ME=`basename $0`
DEBUG=0
RMTMP=1

# ##

while getopts vhk o
do case "$o" in
	h)	echo "usage: $ME [-h] [-v] <testsrc.xml>"
		echo "       -h   show this help message"
		echo "       -v   show more output"
		echo "	     -k   keep temporary files"
		exit 0
		;;
	d)	DEBUG=1   ;;
	k)	RMTMP=0   ;;
    esac
done
shift $(($OPTIND-1))
TESTSRC="$1"

if [ -z "$TESTSRC" -o ! -f "$TESTSRC" ]; then
    echo "$ME: no filename (-h for help)" 1>&2
    exit 1
fi

# ##

LAYOUT_OPTION=`$GETDESC $TESTSRC | $AWK -v mode=setlayout -f description.awk`
EVAL_OPTION=`$GETDESC $TESTSRC | $AWK -v mode=eval -f description.awk`
TESTPRE=`expr "$TESTSRC" : '\(.*\)\.xml'`

TESTOUT="$TESTPRE.log"
DIFFOUT="$TESTPRE.diff"
TESTOUT_EXPECTED="$TESTPRE-expected.log"
ERROR_EXPECTED=`$GETDESC $TESTSRC | $AWK -v mode=error -f description.awk`

$GETDESC $TESTSRC | $AWK -v mode=stderr -f description.awk > $TESTOUT_EXPECTED

echo +---------------------------------------------------------------------
echo "| $TESTSRC"
$GETDESC $TESTSRC | $AWK -v mode=description -f description.awk | sed 's/^/| /'

CMD="$URPAL "$LAYOUT_OPTION" "$EVAL_OPTION" --input=$TESTSRC --output=$TESTPRE-flip.xml"
if [ $DEBUG -eq 1 ]; then echo $CMD; fi
$CMD 2> $TESTOUT
ERROR=$?

diff -b $TESTOUT_EXPECTED $TESTOUT > $DIFFOUT
DIFFRESULT=$?

if [ $ERROR -ne $ERROR_EXPECTED ]; then
    echo "| FAILED (expected $ERROR_EXPECTED, got $ERROR)"
elif [ $DIFFRESULT -ne 0 ]; then
    echo "| FAILED (difference on stderr expected< >actual)"
    cat $DIFFOUT | grep '^[><-]'
else
    echo "| PASSED"
    if [ `stat -f %z $TESTOUT` -eq 0 ]; then rm $TESTOUT; fi
fi

if [ $RMTMP -eq 1 ]; then rm $TESTOUT_EXPECTED $DIFFOUT; fi

