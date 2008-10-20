module Blog.Processors
    (canonicalUri
    ) where

import Data.List (isPrefixOf)
import Web.Request
import Web.Response
import qualified Blog.Settings as Settings


canonicalUri :: Request -> IO (Maybe Response)
canonicalUri req =
    let uri' = requestUriRaw req
    in return $ case uri' of
                  Nothing -> Nothing
                  Just uri -> if Settings.prog_uri `isPrefixOf` uri
                     then let canonUri = Settings.root_url ++ drop (length Settings.prog_uri + length "/") uri
                          in Just $ redirectResponse canonUri
                     else Nothing
