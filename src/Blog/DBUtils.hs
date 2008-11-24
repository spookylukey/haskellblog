module Blog.DBUtils where

import Blog.Utils (regexReplace)
import Database.HDBC
import GHC.Unicode (toLower)
import qualified Data.ByteString.Lazy.Char8 as BL

slugFromTitle title = map toLower $ BL.unpack $
                      regexReplace (BL.pack "-+$") (BL.pack "") $
                      regexReplace (BL.pack "[^A-Za-z0-9]+") (BL.pack "-") (BL.pack title)

makeSlugGeneric cn title table = makeSlugGeneric' cn (slugFromTitle title) table 1
makeSlugGeneric' cn slugBase table iter = do
  let slugAttempt =  (slugBase ++ makeSuffix iter);
  [[SqlString c]] <- quickQuery cn ("SELECT count(slug) FROM " ++ table ++ " WHERE slug = ?") [toSql slugAttempt];
  case c of
    "0" -> return slugAttempt
    _   -> makeSlugGeneric' cn slugBase table (iter + 1)

 where
   makeSuffix 1 = ""
   makeSuffix n = show n
