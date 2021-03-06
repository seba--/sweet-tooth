#!/bin/sh

# example call
#   ./sweet-tooth.sh  ~/projects/sugarj/case-studies/java-pet-store/src/java  xml/Sugar.sugj  desugar-xml

ProjectPATH=$1
SUGARJ_BIN=$ProjectPATH/bin
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

# echo "compile sweet tooth Stratego programs"
# mkdir -p $SCRIPTDIR/bin
# $STRJ -o $SCRIPTDIR/bin/extract_generation_type.java -i $SCRIPTDIR/stratego/extract-generation-type.str -I $SUGARJ_LIB
# $STRJ -m matching-main -o $SCRIPTDIR/bin/matching.java -i $SCRIPTDIR/stratego/matching.str -I $SUGARJ_LIB -la stratego-xtc -la stratego-sglr
# javac -cp $SCRIPTDIR/bin:$STRATEGO_JAR -d $SCRIPTDIR/bin $SCRIPTDIR/bin/*.java
# echo

# echo "compile sugarj module"
# SUGARJ_CACHE=/tmp/sugarj-cache
# mktemp -d -q /tmp/sugarj-out > /dev/null 
# mktemp -d -q /tmp/sugarj-cache > /dev/null 
# $SUGARJ --cache $SUGARJ_CACHE -l java -d $SUGARJ_BIN --sourcepath $ProjectPATH/src $SRCFILE
# echo

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

# echo "match pattern against target file" 
# MATCH_PAIR=`mktemp -t match-pair-XXXXX.aterm`
# #$SUGARJ --cache $SUGARJ_CACHE -l java -d $SUGARJ_BIN --sourcepath $MATCHPATH $MATCHFILE
# Name1=`basename $MATCHFILE .sugj`
# Name2=`basename $Name1 .java`
# MODEL=$SUGARJ_BIN/`dirname $MATCHFILE`/$Name2.model

# echo "Use model $MODEL"

# echo "(" > $MATCH_PAIR
# cat $TYPEFILE >> $MATCH_PAIR
# echo "," >> $MATCH_PAIR
# cat $MODEL >> $MATCH_PAIR
# echo ")" >> $MATCH_PAIR

# java -cp $SCRIPTDIR/bin:$STRATEGO_JAR matching -i $MATCH_PAIR
# echo

