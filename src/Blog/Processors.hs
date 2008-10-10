module Blog.Processors
    (canonicalUri
    ) where

import Data.List (isPrefixOf)
import Web.Request
import Web.Response
import qualified Blog.Settings as Settings


canonicalUri view req =
    let uri' = requestUriRaw req
    in case uri' of
         Nothing -> view req
         Just uri -> if Settings.prog_uri `isPrefixOf` uri
                     then let canonUri = Settings.root_url ++ drop (length Settings.prog_uri + length "/") uri
                          in return $ Just $ redirectResponse canonUri
                     else view req
