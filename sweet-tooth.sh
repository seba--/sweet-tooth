#!/bin/sh

# example call
#   ./sweet-tooth.sh  ~/projects/sugarj/case-studies/java-pet-store/src/java  xml/Sugar.sugj  desugar-xml

SRCPATH=$1
SRCFILE=$2
DESUGARING=$3
MATCHPATH=$4
MATCHFILE=$5

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
$STRJ -o $SCRIPTDIR/bin/matching.java -i $SCRIPTDIR/stratego/matching.str -I $SUGARJ_LIB
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
NORM=`mktemp -t desugar-XXXXX.str`
$STRJ -F -o $NORM -i $SUGARJ_BIN/`dirname $SRCFILE`/`basename $SRCFILE .sugj`.str -I $SUGARJ_BIN -I $SUGARJ_LIB -I $SUGARJ_LIB
echo "  Wrote normalized Stratego code"
echo "    $NORM"
echo

echo "extract type" 
TYPEFILE=`mktemp -t generation-type-XXXXX.str`
java -Xmx2048m -cp $SCRIPTDIR/bin:$STRATEGO_JAR extract_generation_type -i $NORM -desugar ${DESUGARING_NORM}_0_0 -o $TYPEFILE
cat $TYPEFILE
echo

echo "match pattern against target file" 
# $SUGARJ --cache $SUGARJ_CACHE -l java -d $SUGARJ_BIN --sourcepath $SRCPATH $TARGETFILE
java -cp $SCRIPTDIR/bin:$STRATEGO_JAR matching -i $SUGARJ_BIN/`dirname $MATCHFILE`/`basename $MATCHFILE .sugj`.model -t $TYPE -desugar ${DESUGARING_NORM}_0_0
echo

