#!/bin/bash

# scalac \
#   -cp "../../scala-parser-combinators_2.13-1.1.2.jar" \
#   -deprecation \
#   -feature \
#   -unchecked \
#   ./*.scala 

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
  -cp ".:$PARSER_COMBINATORS_JAR" \
  $MAIN_CLASS
