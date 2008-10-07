-- | General utility functions that do not depend on other functions
-- in Web modules
module Web.GenUtils

where


-- | Apply a list of transformation functions to an object
apply :: [a -> a] -- ^ List of functions
      -> a        -- ^ Initial value
      -> a
apply fs init = foldl (flip ($)) init fs

-- | Same as apply with arguments flipped
with = flip apply
