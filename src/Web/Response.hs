module Web.Response (Response, content, headers, addContent, textResponse, htmlResponse, utf8HtmlResponse, emptyResponse) where

-- Mainly borrowed from Network.CGI.Protocol

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.CGI.Protocol (Headers, HeaderName(HeaderName))
import Network.CGI (ContentType(ContentType), showContentType)

data Response = Response {
      content :: ByteString
    , headers :: Headers
    }

emptyResponse = Response { content = BS.empty, headers = [] }

addContent :: ByteString -> Response -> Response
addContent c resp = resp { content = c }

{-
TODO
 - add utility functions for writing HTML
 - add encoding/charset to response, so that it can automatically
   convert HTML to the correct encoding.
-}


-- Utility functions for typical defaults

contentTypeName = HeaderName "Content-type"
textContent charset = ContentType "text" "plain" [("charset", charset)]
htmlContent charset = ContentType "text" "html" [("charset", charset)]

textResponse charset = Response {
                         content = BS.pack ""
                       , headers = [(contentTypeName, showContentType $ textContent charset)]
                       }

htmlResponse charset = Response {
                         content = BS.pack ""
                       , headers = [(contentTypeName, showContentType $ htmlContent charset)]
                       }

{-

TODO

utf8HtmlResponse should probably accept Html as an argument and return
the full Response, rather than returning an empty Response.

Need to think more about constructors for Responses and how the API
should work.

-}

utf8HtmlResponse = htmlResponse "utf-8"
