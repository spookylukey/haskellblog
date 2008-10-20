module Blog.Processors
    (canonicalUri
    ) where

import Data.List (isPrefixOf)
import Web.Request
import Web.Response
import qualified Blog.Settings as Settings


canonicalUri :: Request -> IO (Maybe Response)
canonicalUri req =
    return $ case requestUriRaw req of
               Nothing -> Nothing
               Just uri | Settings.prog_uri `isPrefixOf` uri
                       -> let canonUri = Settings.root_url ++ drop (length Settings.prog_uri + length "/") uri
                               in Just $ redirectResponse canonUri
               _       -> Nothing
