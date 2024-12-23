#!/bin/bash

set -e

PARSER_COMBINATORS_JAR="../../scala-parser-combinators_2.13-1.1.2.jar"

MAIN_CLASS="DeskEvaluator"

scalac \
  -cp "$PARSER_COMBINATORS_JAR" \
  -deprecation \
  -feature \
  -unchecked \
  ./*.scala

scala \
  -cp .:$PARSER_COMBINATORS_JAR \
  $MAIN_CLASS \
  test.desk
