module Category where
{

    data Category = Category { id :: Int,
                               name :: String,
                               slug :: String
                             } deriving (Show, Eq)

}
