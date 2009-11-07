#!/bin/sh

# These depend on the migration having been done

# Check that a unicode char in a post came out right.
curl http://lukeplant_local/blog/posts/building-ghc-is-fun/ 2> /dev/null | grep 'bug #3639</a> — it won' > /dev/null || { echo "Problem with unicode chars in posts"; }
# and one in a comment
curl http://lukeplant_local/blog/posts/haskell-string-support/ 2> /dev/null | grep '&nbsp;&nbsp;&nbsp;&nbsp;s = &quot;λ&quot;' > /dev/null || { echo "Problem with unicode chars in comments"; }
# and one from the pagestart.st template
curl http://lukeplant_local/blog/ 2> /dev/null | grep 'Recent posts « All' > /dev/null || { echo "Problem with unicode in templates"; }
