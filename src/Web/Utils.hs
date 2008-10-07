module Web.Utils ( addHtml
                 )

where

import Text.XHtml (renderHtml)
import Web.Response (addContent)
import Web.GenUtils (utf8)

-- Utility functions
addHtml html resp = addContent (utf8 $ renderHtml html) resp
