module Web.Framework.Processors
    ( addSlashRedirectProcessor
    )

where

import Data.List (isSuffixOf)
import Web.Request
import Web.Response

-- TODO -- need to include query string, and think about how to handle
-- POSTs etc
addSlashRedirectProcessor view req =
    let uri = requestUriRaw req
    in case uri of
        Nothing -> view req -- Can't do a redirect if we don't know original URI
        Just "" -> view req -- Don't redirect if empty
        Just x ->  if not ("/" `isSuffixOf` x)
                   then return $ Just $ redirectResponse (x ++ "/")
                   else view req
