#!/bin/sh
DIR=`dirname $0`
rm $DIR/test.db 2> /dev/null
sqlite3 $DIR/test.db < $DIR/../schema/install.sql
sqlite3 $DIR/test.db < $DIR/testdata.sql
runhaskell -i$DIR:$DIR/../src $DIR/Main.hs
