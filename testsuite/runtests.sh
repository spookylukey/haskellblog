#!/bin/sh
DIR=`dirname $0`
rm $DIR/test.db 2> /dev/null
runhaskell -i$DIR:$DIR/../src $DIR/Main.hs
