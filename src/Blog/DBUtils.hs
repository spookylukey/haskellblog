module Blog.DBUtils where

import Blog.Utils (regexReplace)
import Database.HDBC
import GHC.Unicode (toLower)
import qualified Data.ByteString.Char8 as B

slugFromTitle title = map toLower $ B.unpack $
                      regexReplace (B.pack "-+$") (B.pack "") $
                      regexReplace (B.pack "[^A-Za-z0-9]+") (B.pack "-") (B.pack title)

makeSlugGeneric cn slugBase table iter = do
  let slugAttempt =  (slugBase ++ makeSuffix iter);
  [[SqlString c]] <- quickQuery cn ("SELECT count(slug) FROM " ++ table ++ " WHERE slug = ?") [toSql slugAttempt];
  case c of
    "0" -> return slugAttempt
    _   -> makeSlugGeneric cn slugBase table (iter + 1)

 where
   makeSuffix 1 = ""
   makeSuffix n = show n
