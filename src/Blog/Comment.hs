module Blog.Comment where


data Comment = Comment {
      uid :: Int
    , post_id :: Int
    , timestamp :: Int
    , name :: String
    , email :: String
    , text_raw :: String
    , text_formatted :: String
    , format_id :: Int
    } deriving (Show, Eq)
