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

touch application.debug.old access.error system.error 
echo "access" > access.error
echo "system" > system.error

scala \
  -cp .:$PARSER_COMBINATORS_JAR \
  $MAIN_CLASS \
  test.ll
