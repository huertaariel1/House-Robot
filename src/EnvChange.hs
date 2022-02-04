module EnvChange
  ( childrenAction1,
    childrenAction,
    moveChild,
    pushObstacles,
    leaveDirt,
    putDirty,
    gimmeSquare3X3,
    gimmeSquare3X3C,
    gimmeSquare3X3Empty,
    validMove,
    getChildren,
  )
where

import EnvElements
import Environment
import System.IO.Unsafe
import Utils

childrenAction :: Int -> Int -> [Elements] -> [Elements]
childrenAction n m env =
  let children = getChildren env env []
   in if null children then env else childrenAction1 n m env children

-- q pasa si esta vacio el array q devuelve children

childrenAction1 :: Int -> Int -> [Elements] -> [Elements] -> [Elements]
childrenAction1 n m env [] = env
childrenAction1 n m env (Child (a, b) c : children) =
  let x = randomNum2 0 5
      r = unsafePerformIO x
   in if r > 0
        then
          let directions = gimmeValidDirections n m env [(a -1, b), (a + 1, b), (a, b -1), (a, b + 1)] [] (a, b)
              l = length directions
           in if l == 0
                then childrenAction1 n m env children
                else
                  let rand = randomNum 0 (l -1)
                      (pr, pc) = directions !! rand
                      child = Child (a, b) c
                      (dr, dc) = (pr - a, pc - b)
                      env1 = moveChild n m env child (pr, pc) (dr, dc)
                   in childrenAction1 n m env1 children
        else childrenAction1 n m env children
childrenAction1 _ _ env _ = env

moveChild :: Int -> Int -> [Elements] -> Elements -> (Int, Int) -> (Int, Int) -> [Elements]
moveChild n m env (Child (a, b) d) (r, c) (dr, dc) =
  let occupied = index env (r, c)
      child = Child (a, b) d
   in case occupied of
        [] -> leaveDirt n m env child (r, c)
        _ ->
          let env1 = pushObstacles env occupied (dr, dc)
           in leaveDirt n m env1 child (r, c)
moveChild _ _ env _ _ _ = env

pushObstacles :: [Elements] -> [Elements] -> (Int, Int) -> [Elements]
pushObstacles env [] _ = env
pushObstacles env [Obstacle (a, b)] (r, c) =
  let env1 = updateCell env (Obstacle (a, b)) (Obstacle (a + r, b + c))
      occupied = index env (a + r, b + c)
   in pushObstacles env1 occupied (r, c)
pushObstacles env _ _ = env

leaveDirt :: Int -> Int -> [Elements] -> Elements -> (Int, Int) -> [Elements]
leaveDirt n m env (Child (a, b) d) (r, c) =
  let env1 = updateCell env (Child (a, b) d) (Child (r, c) d)
      square = gimmeSquare3X3 n m (r, c) [(a -1, b), (a + 1, b), (a, b -1), (a, b + 1), (a + 1, b -1), (a -1, b + 1), (a -1, b -1), (a + 1, b + 1), (a, b)]
      emptyCells = gimmeSquare3X3Empty env1 square
      l = length emptyCells
      csquare = gimmeSquare3X3C env1 square
      lc = length csquare + 1
      maxDirty
        | lc == 1 = 1
        | lc == 2 = 3
        | otherwise = 6
      maxDirtyPossible = min l maxDirty
      dirty = randomNum 0 maxDirtyPossible
   in putDirty env1 dirty emptyCells
leaveDirt _ _ env _ _ = env

putDirty :: [Elements] -> Int -> [(Int, Int)] -> [Elements]
putDirty env 0 _ = env
putDirty env d cells =
  let l = length cells
      rand = randomNum 0 (l -1)
      pos = cells !! rand
      ncells = removeCell pos cells False
      r = fst pos
      c = snd pos
      nenv = addCell env (Dirty (r, c))
   in putDirty nenv (d -1) ncells

gimmeValidDirections :: Int -> Int -> [Elements] -> [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
gimmeValidDirections _ _ _ [] d _ = d
gimmeValidDirections n m env ((a, b) : directions) d (oa, ob) =
  if let occupied = index env (a, b)
         (da, db) = (a - oa, b - ob)
      in boundsEnv n m a b && validMove n m env occupied (da, db)
    then gimmeValidDirections n m env directions (d ++ [(a, b)]) (oa, ob)
    else gimmeValidDirections n m env directions d (oa, ob)

gimmeSquare3X3C :: [Elements] -> [(Int, Int)] -> [(Int, Int)]
gimmeSquare3X3C env xs = do
  x <- xs
  let re = fst x
      ce = snd x
      occupied = index env (re, ce)
   in case occupied of
        [Child (re, ce) False] -> y ++ [x]
        _ -> y
  where
    y = []

gimmeSquare3X3Empty :: [Elements] -> [(Int, Int)] -> [(Int, Int)]
gimmeSquare3X3Empty env xs = do
  x <- xs
  if ( let re = fst x
           ce = snd x
           occupied = index env (re, ce)
        in null occupied
     )
    then y ++ [x]
    else y
  where
    y = []

gimmeSquare3X3 :: Int -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
gimmeSquare3X3 n m (r, c) xs = do
  x <- xs
  if ( let re = fst x
           ce = snd x
        in not (re == r && ce == c) && boundsEnv n m re ce
     )
    then y ++ [x]
    else y
  where
    y = []

validMove :: Int -> Int -> [Elements] -> [Elements] -> (Int, Int) -> Bool
validMove n m _ [] _ = True
validMove n m env [Obstacle (a, b)] (r, c) = boundsEnv n m (a + r) (b + c) && (let occupied = index env (a + r, b + c) in validMove n m env occupied (r, c))
validMove n m _ _ _ = False

getChildren :: [Elements] -> [Elements] -> [Elements] -> [Elements]
getChildren envO [] cenv = cenv
getChildren envO (Child (a, b) False : env) cenv = if let envO1 = removeCell (Child (a, b) False) envO False in not (taken envO1 (a, b)) then let cenv1 = cenv ++ [Child (a, b) False] in getChildren envO env cenv1 else getChildren envO env cenv
getChildren envO (_ : env) cenv = getChildren envO env cenv
