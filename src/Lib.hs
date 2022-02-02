module Lib
  ( sayHello,
  )
where

import Environment

sayHello :: String -> IO ()
sayHello x = let y = initEnv 4 5 3 3 3 3 in print y

-- comment