module Tests.Blog.Formats where

import Test.HUnit
import Blog.Formats

pt = getFormatter Plaintext

testFormatPlaintext = "Hello<br />\ndef foo(bar):<br />\n&nbsp;&nbsp;&nbsp;&nbsp;# some code &amp; comments<br />\n<br />\n&lt;snip&gt;" ~=?
                      pt "Hello\ndef foo(bar):\n    # some code & comments\n\n<snip>"

testFormatPlaintextLink1 = "<a href=\"http://foo.com\">http://foo.com</a> x" ~=?
                           pt "http://foo.com x"

testFormatPlaintextLink2 = "<a href=\"http://foo.com/\">http://foo.com/</a> x" ~=?
                           pt "http://foo.com/ x"

testFormatPlaintextLink3 = "<a href=\"http://foo.com/bar?what=why\">http://foo.com/bar?what=why</a> x" ~=?
                           pt "http://foo.com/bar?what=why x"

tests = test [ testFormatPlaintext
             , testFormatPlaintextLink1
             , testFormatPlaintextLink2
             , testFormatPlaintextLink3]
