#!/bin/sh
DIR=`dirname $0`
rm $DIR/test.db 2> /dev/null
runhaskell -i$DIR/tests:$DIR/../src $DIR/tests/Main.hs
