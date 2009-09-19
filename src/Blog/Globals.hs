module Blog.Globals ( csrfProtectionProcessor
                    , mkCsrfField
                    )

where

import Ella.Response (Cookie(..))
import Ella.Processors.Security (mkCSRFProtection, defaultCSRFRejectView, CSRFProtection(..))
import qualified Blog.Settings as Settings

csrfProtection = mkCSRFProtection (Cookie { cookieName = "csrf"
                                          , cookieValue = undefined
                                          , cookieDomain = Nothing
                                          , cookiePath = Just "/"
                                          , cookieExpires = Nothing
                                          , cookieSecure = False })
                 defaultCSRFRejectView Settings.secret

csrfProtectionProcessor = csrfViewProcessor csrfProtection
mkCsrfField = csrfTokenField csrfProtection
