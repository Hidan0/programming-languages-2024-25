#!/bin/bash

set -e

PARSER_COMBINATORS_JAR="../../scala-parser-combinators_2.13-1.1.2.jar"

MAIN_CLASS="Main"

scalac \
  -cp "$PARSER_COMBINATORS_JAR" \
  -deprecation \
  -feature \
  -unchecked \
  ./*.scala

scala \
  -cp .:$PARSER_COMBINATORS_JAR \
  $MAIN_CLASS \
  "10" \
  "(2 * 5)" \
  "((2 + 7) + ((3 + 9) + 4))" \
  "((1 * 7) + (7 * ((3 + 9) + 5)))" \
  "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))" \
