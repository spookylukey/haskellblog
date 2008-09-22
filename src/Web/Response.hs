module Web.Response (Response, content, headers, addContent, textResponse, htmlResponse, emptyResponse) where

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


-- Utility functions for typical defaults

contentTypeName = HeaderName "Content-type"
textContent charset = ContentType "text" "plain" [("charset", charset)]
htmlContent charset = ContentType "text" "html" [("charset", charset)]

utf8HtmlContent = htmlContent "utf-8"

textResponse charset = Response {
                         content = BS.pack ""
                       , headers = [(contentTypeName, showContentType $ textContent charset)]
                       }

htmlResponse charset = Response {
                         content = BS.pack ""
                       , headers = [(contentTypeName, showContentType $ htmlContent charset)]
                       }
