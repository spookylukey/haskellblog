module Blog.Post where
{
  data Post = Post {
        id :: Int,
        title :: String,
        slug :: String,
        post_raw :: String,
        post_formatted :: String,
        summary_raw :: String,
        summary_formatted :: String,
        format_id :: Int,
        timestamp :: Int,
        comments_open :: Bool
      } deriving (Show, Eq)
}

