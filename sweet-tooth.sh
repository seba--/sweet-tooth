#!/bin/sh

SRCPATH=$1
SRCFILE=$2
DESUGARING=$3

SUGARJ_BASE=/Users/seba/projects/sugarj
SUGARJ=$SUGARJ_BASE/deployment/cli-scripts/sugarj-dev
SUGARJ_STDLIB=$SUGARJ_BASE/stdlib
SUGARJ_JAVALIB=$SUGARJ_BASE/language-libraries/java/src
STRATEGO_JAR=/Users/seba/Library/eclipse-sugarj/plugins/org.strategoxt.strj_0.17.92.201212201448/java/strategoxt.jar
STRJ="java -jar $STRATEGO_JAR"

## compile sugarj module
BIN=/tmp/sugarj-out
CACHE=/tmp/sugarj-cache
mktemp -d -q /tmp/sugarj-out > /dev/null 
mktemp -d -q /tmp/sugarj-cache > /dev/null 
$SUGARJ --cache $CACHE -l java -d $BIN --sourcepath $SRCPATH $SRCFILE

## normalize resulting Stratego file
NORM=`mktemp -t desugar-XXXXX.str`
$STRJ -F -o $NORM -i $BIN/`dirname $SRCFILE`/`basename $SRCFILE .sugj`.str -I $BIN -I $SUGARJ_STDLIB -I $SUGARJ_JAVALIB
pp-stratego -a -i $NORM
