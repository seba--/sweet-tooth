#!/bin/sh

# example call
#   ./sweet-tooth.sh  ~/projects/sugarj/case-studies/java-pet-store/src/java  xml/Sugar.sugj  desugar-xml

SRCPATH=$1
SRCFILE=$2
DESUGARING=$3

DESUGARING_NORM=`echo $DESUGARING | sed -e "s/\\-/\\_/g"`
SCRIPTDIR=$(dirname $0)
LIB=$SCRIPTDIR/lib
SUGARJ_BASE=$LIB/sugarj
SUGARJ=$SUGARJ_BASE/bin/sugarj
SUGARJ_LIB=$SUGARJ_BASE/lib
STRATEGO_JAR=$LIB/strategoxt.jar
STRJ="java -jar $STRATEGO_JAR"

echo "compile sweet tooth Stratego programs"
mkdir -p $SCRIPTDIR/bin
$STRJ -o $SCRIPTDIR/bin/extract_generation_type.java -i $SCRIPTDIR/stratego/extract-generation-type.str -I $SUGARJ_LIB
javac -cp $SCRIPTDIR/bin:$STRATEGO_JAR -d $SCRIPTDIR/bin $SCRIPTDIR/bin/*.java
echo

echo "compile sugarj module"
SUGARJ_BIN=/tmp/sugarj-out
SUGARJ_CACHE=/tmp/sugarj-cache
mktemp -d -q /tmp/sugarj-out > /dev/null 
mktemp -d -q /tmp/sugarj-cache > /dev/null 
$SUGARJ --cache $SUGARJ_CACHE -l java -d $SUGARJ_BIN --sourcepath $SRCPATH $SRCFILE
echo

echo "normalize resulting Stratego file"
#NORM=/var/folders/n_/ht4vd47d3z7f9t3xzw2y1n1w0000gn/T/desugar.str.kFQgEIu4
NORM=`mktemp -t desugar.str`
$STRJ -F -o $NORM --lib -i $SUGARJ_BIN/`dirname $SRCFILE`/`basename $SRCFILE .sugj`.str -I $SUGARJ_BIN -I $SUGARJ_LIB -I $SUGARJ_LIB
echo "Wrote normalized Stratego code"
echo "  $NORM"
echo

echo "extract type " 
java -cp $SCRIPTDIR/bin:$STRATEGO_JAR extract_generation_type -i $NORM -desugar ${DESUGARING_NORM}_0_0
echo
