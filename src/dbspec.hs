module Main where

import Database.HaskellDB
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec


opts = DBOptions {useBString = False}

blogdb = DBInfo {dbname = "ctest", opts = testopts, tbls = [blogtable]}


blogtable = TInfo {tname = "blog", cols = []}
idcol = CInfo {cname = "id", descr = ()}


testcol11 = CInfo {cname = "ctestcol11", descr = (IntT,False)}
testcol12 = CInfo {cname = "ctestcol12", descr = (BStrT 8,True)}

testcol21 = CInfo {cname = "ctestcol21", descr = (BStrT 6,False)}
testcol22 = CInfo {cname = "ctestcol22", descr = (IntT,True)}
