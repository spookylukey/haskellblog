module Blog.DBUtils ( makeSlugGeneric
                    , slugFromTitle
                    , getDbId
                    , sqlInIds
                    , pagedQuery
                    )

where

import Blog.Utils (regexReplace)
import Database.HDBC
import GHC.Unicode (toLower)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as List

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

getDbId :: (IConnection conn, SqlType a) => conn -> IO a
getDbId cn =
    do
      [[newid]] <- quickQuery' cn "SELECT last_insert_rowid();" []
      return $ fromSql newid

-- SQL stuff
sqlInIds :: [Int] -> String
sqlInIds ids = "(" ++ (concat $ List.intersperse "," $ map show ids) ++ ")"

addLimitOffset sql limitOffset =
    BL.unpack $ regexReplace (" \\$LIMITOFFSET") (BL.pack $ " " ++ limitOffset) (BL.pack sql)

-- return 'LIMIT/OFFSET' for a page (1 indexed), with an extra row
-- which allows us to tell if there are more records
makePagingLimitOffset page size =
    let limit = size + 1
        offset = (page - 1) * size
    in "LIMIT " ++ (show limit) ++ " OFFSET " ++ (show offset)

-- | Get a page of results, and a boolean which is True if there are more rows
--
-- The query must contain "$LIMITOFFSET" in an appropriate place to be replaced
-- with the actual limit/offset clause
pagedQuery :: (IConnection conn) =>
              conn -> [Char] -> [SqlValue] -> Int -> Int -> IO ([[SqlValue]], Bool)
pagedQuery cn sql params page pagesize =
    let limitOffset = makePagingLimitOffset page pagesize
        q = addLimitOffset sql limitOffset
    in do
      res <- quickQuery' cn q params
      let (recs,rest) = splitAt pagesize res
      return (recs, not $ null rest)
