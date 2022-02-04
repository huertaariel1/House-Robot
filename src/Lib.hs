module Lib
  ( sayHello,
  )
where

import Agents
import EnvElements
import Environment

sayHello :: String -> IO ()
sayHello x = let y = bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) True, Child (4, 4) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] (1, 2) in print y

-- comment