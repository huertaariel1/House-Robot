{-# OPTIONS_GHC -Wno-deprecations #-}

module Environment
  ( index,
    removeCell,
    updateCell,
    boundsEnv,
    randomNum,
    placePlaypen,
    placeRobots,
    placeChilds,
    placeDirty,
    placeObstacle,
    taken,
    initEnv,
    randomNum1,
  )
where

import EnvElements
import System.IO.Unsafe
import System.Random

randomNum :: Int -> Int -> Int
{-# NOINLINE randomNum #-}
randomNum min max = unsafePerformIO (getStdRandom (randomR (min, max)))

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound s = (i `mod` bound, s') where (i, s') = next s

randomNum1 :: Int -> Int
randomNum1 x = fst (nextBounded x (mkStdGen x))

initEnv :: Int -> Int -> Int -> Int -> Int -> Int -> [Elements]
initEnv n m r c o d = env
  where
    env1 = placePlaypen n m c
    env2 = placeChilds n m c env1
    env3 = placeRobots n m r env2
    env4 = placeObstacle n m o env3
    env = placeDirty n m d env4

placePlaypen :: Int -> Int -> Int -> [Elements]
placePlaypen n m c = if boundsEnv n m rr rc then placePlaypen1 n m rr rc (c - 1) [Playpen (rr, rc)] else placePlaypen n m c
  where
    rr = randomNum 1 n
    rc = randomNum 1 m

placePlaypen1 :: Int -> Int -> Int -> Int -> Int -> [Elements] -> [Elements]
placePlaypen1 n m lr lc 0 env = env
placePlaypen1 n m lr lc c env =
  let r = randomNum 1 4
   in case r of
        1 -> if boundsEnv n m (lr - 1) lc && not (taken env (lr - 1, lc)) then (let env1 = env ++ [Playpen (lr - 1, lc)] in placePlaypen1 n m (lr - 1) lc (c - 1) env1) else placePlaypen1 n m lr lc c env
        2 -> if boundsEnv n m (lr + 1) lc && not (taken env (lr + 1, lc)) then (let env2 = env ++ [Playpen (lr + 1, lc)] in placePlaypen1 n m (lr + 1) lc (c - 1) env2) else placePlaypen1 n m lr lc c env
        3 -> if boundsEnv n m lr (lc - 1) && not (taken env (lr, lc - 1)) then (let env3 = env ++ [Playpen (lr, lc - 1)] in placePlaypen1 n m lr (lc - 1) (c - 1) env3) else placePlaypen1 n m lr lc c env
        _ -> if boundsEnv n m lr (lc + 1) && not (taken env (lr, lc + 1)) then (let env4 = env ++ [Playpen (lr, lc + 1)] in placePlaypen1 n m lr (lc + 1) (c - 1) env4) else placePlaypen1 n m lr lc c env

placeChilds :: Int -> Int -> Int -> [Elements] -> [Elements]
placeChilds n m 0 env = env
placeChilds n m c env = if boundsEnv n m rr rc && not (taken env (rr, rc)) then placeChilds n m (c -1) (env ++ [Child (rr, rc) False]) else placeChilds n m c env
  where
    rr = randomNum 1 n
    rc = randomNum 1 m

placeRobots :: Int -> Int -> Int -> [Elements] -> [Elements]
placeRobots n m 0 env = env
placeRobots n m r env = if boundsEnv n m rr rc && not (taken env (rr, rc)) then placeRobots n m (r -1) (env ++ [Robot (rr, rc) False]) else placeRobots n m r env
  where
    rr = randomNum 1 n
    rc = randomNum 1 m

placeObstacle :: Int -> Int -> Int -> [Elements] -> [Elements]
placeObstacle n m 0 env = env
placeObstacle n m o env = if boundsEnv n m rr rc && not (taken env (rr, rc)) then placeObstacle n m (o -1) (env ++ [Obstacle (rr, rc)]) else placeObstacle n m o env
  where
    rr = randomNum 1 n
    rc = randomNum 1 m

placeDirty :: Int -> Int -> Int -> [Elements] -> [Elements]
placeDirty n m 0 env = env
placeDirty n m d env = if boundsEnv n m rr rc && not (taken env (rr, rc)) then placeDirty n m (d -1) (env ++ [Dirty (rr, rc)]) else placeDirty n m d env
  where
    rr = randomNum 1 n
    rc = randomNum 1 m

taken :: [Elements] -> (Int, Int) -> Bool
taken env (r, c) = case xs of
  [] -> False
  _ -> True
  where
    xs = index env (r, c)

index :: [Elements] -> (Int, Int) -> [Elements]
index xs (r, c) = do
  x <- xs
  if ( let re = row x
           ce = column x
        in re == r && ce == c
     )
    then y ++ [x]
    else y
  where
    y = []

removeCell :: Elements -> [Elements] -> [Elements]
removeCell _ [] = []
removeCell y (x : xs)
  | x == y = removeCell y xs
  | otherwise = x : removeCell y xs

updateCell :: [Elements] -> Elements -> Elements -> [Elements]
updateCell env oEl nEl = do
  nEnv ++ [nEl]
  where
    nEnv = removeCell oEl env

addCell :: [Elements] -> Elements -> [Elements]
addCell env nEl = env ++ [nEl]

boundsEnv :: Int -> Int -> Int -> Int -> Bool
boundsEnv n m r c = (r >= 1 && c >= 1) && not (r > n || c > m)