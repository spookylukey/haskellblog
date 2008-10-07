module Web.Framework.Processors
    ( addSlashRedirectProcessor
    )

where

import Data.List (isSuffixOf)
import Web.Request
import Web.Response

addSlashRedirectProcessor view req =
    if not ("/" `isSuffixOf` pathInfo req)
    then
        return $ Just $ redirectResponse (pathInfo req ++ "/")
        else view req
