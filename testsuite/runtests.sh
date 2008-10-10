#!/bin/sh
DIR=`dirname $0`
runhaskell -i$DIR/tests:$DIR/../src $DIR/tests/Main.hs
