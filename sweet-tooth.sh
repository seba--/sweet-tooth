#!/bin/sh

SRCPATH=$1
SRCFILE=$2
DESUGARING=$3

DESUGARING_NORM=`echo $DESUGARING | sed -e "s/\\-/\\_/g"`
SCRIPTDIR=$(dirname $0)
SUGARJ_BASE=/Users/seba/projects/sugarj
SUGARJ=$SUGARJ_BASE/deployment/cli-scripts/sugarj-dev
SUGARJ_STDLIB=$SUGARJ_BASE/stdlib
SUGARJ_JAVALIB=$SUGARJ_BASE/language-libraries/java/src
STRATEGO_JAR=/Users/seba/Library/eclipse-sugarj/plugins/org.strategoxt.strj_0.17.92.201212201448/java/strategoxt.jar
STRJ="java -jar $STRATEGO_JAR"

echo "compile sweet tooth Stratego programs"
mkdir -p $SCRIPTDIR/bin
$STRJ -o $SCRIPTDIR/bin/extract_generation_type.java -i $SCRIPTDIR/stratego/extract-generation-type.str -I $SUGARJ_STDLIB
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
NORM=`mktemp -t desugar.str`
$STRJ -F -o $NORM -i $SUGARJ_BIN/`dirname $SRCFILE`/`basename $SRCFILE .sugj`.str -I $SUGARJ_BIN -I $SUGARJ_STDLIB -I $SUGARJ_JAVALIB
echo "Wrote normalized Stratego code"
echo "  $NORM"
echo

echo "extract type " 
java -cp $SCRIPTDIR/bin:$STRATEGO_JAR extract_generation_type -i $NORM -desugar ${DESUGARING_NORM}_0_0
echo
