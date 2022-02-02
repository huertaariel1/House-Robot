module EnvElements where

data Elements
  = Dirty (Int, Int)
  | Obstacle (Int, Int)
  | Playpen (Int, Int)
  | Child (Int, Int) Bool
  | Robot (Int, Int) Bool
  deriving (Show, Eq)

row :: Elements -> Int
row (Dirty (a, b)) = a
row (Obstacle (a, b)) = a
row (Playpen (a, b)) = a
row (Child (a, b) c) = a
row (Robot (a, b) c) = a

column :: Elements -> Int
column (Dirty (a, b)) = b
column (Obstacle (a, b)) = b
column (Playpen (a, b)) = b
column (Child (a, b) c) = b
column (Robot (a, b) c) = b

carried :: Elements -> Maybe Bool
carried (Child (a, b) c) = Just c
carried (Robot (a, b) c) = Just c
carried _ = Nothing