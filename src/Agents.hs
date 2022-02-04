module Agents
  ( robotAction,
    robotAction1,
    pickChild,
    dropChild,
    cleanDirt,
    gimmePositions,
    bfs,
    bfs1,
    bfsHelper,
    findNearestChild,
    findNearestDirty,
    findNearestPlaypen,
    minNeighbor,
    buildPath,
    buildPath2,
    getRobots,
    getPlaypen,
    getDirty,
  )
where

import Data.Matrix
import EnvChange
import EnvElements
import Environment
import Utils

robotAction :: Int -> Int -> [Elements] -> [Elements]
robotAction n m env =
  let [Robot (a, b) c] = getRobots env []
      occupied = index env (a, b)
      inCell = removeCell (Robot (a, b) c) occupied False
   in robotAction1 n m env (Robot (a, b) c) inCell

robotAction1 :: Int -> Int -> [Elements] -> Elements -> [Elements] -> [Elements]
robotAction1 n m env (Robot (a, b) False) [Dirty (e, f)] = cleanDirt env (Robot (a, b) False) (Dirty (e, f))
robotAction1 n m env (Robot (a, b) True) [Playpen (e, f), Child (g, h) True] = dropChild env (Robot (a, b) True) (Child (g, h) True)
robotAction1 n m env (Robot (a, b) True) [Child (e, f) True, Playpen (g, h)] = dropChild env (Robot (a, b) True) (Child (e, f) True)
robotAction1 n m env (Robot (a, b) True) _ =
  let vMatrix = bfs n m env (a, b)
      pp = getPlaypen env env []
      (pr, pc) = findNearestPlaypen pp vMatrix (n * m) (a, b)
      d = vMatrix ! (pr, pc)
      (nr, nc) = buildPath2 (pr, pc) vMatrix d
      env1 = updateCell env (Robot (a, b) True) (Robot (nr, nc) True)
      env2 = updateCell env1 (Child (a, b) True) (Child (nr, nc) True)
   in env2
robotAction1 n m env (Robot (a, b) False) _ =
  let vMatrix = bfs n m env (a, b)
      dd = getDirty env []
      (dr, dc) = findNearestDirty dd vMatrix (n * m) (a, b)
      distDirty = vMatrix ! (dr, dc)
      pp = getPlaypen env env []
      (nr, nc) =
        if null pp
          then buildPath (dr, dc) vMatrix distDirty
          else
            let children = getChildren env env []
             in if null children
                  then buildPath (dr, dc) vMatrix distDirty
                  else
                    let (cr, cc) = findNearestChild children vMatrix (n * m) (a, b)
                        distChild = vMatrix ! (cr, cc)
                        (pr, pc) = findNearestPlaypen pp vMatrix (n * m) (a, b)
                     in if distDirty >= distChild && (pr, pc) /= (a, b)
                          then buildPath (cr, cc) vMatrix distChild
                          else buildPath (dr, dc) vMatrix distDirty
      occupied = index env (nr, nc)
      env1 = case occupied of
        [Child (nr, nc) False] -> let env2 = updateCell env (Robot (a, b) False) (Robot (nr, nc) True) in updateCell env2 (Child (nr, nc) False) (Child (nr, nc) True)
        _ -> updateCell env (Robot (a, b) False) (Robot (nr, nc) False)
   in env1

pickChild :: [Elements] -> Elements -> Elements -> [Elements]
pickChild env (Robot (a, b) False) (Child (c, d) False) =
  let env1 = updateCell env (Robot (a, b) False) (Robot (c, d) True)
      env2 = updateCell env1 (Child (c, d) False) (Child (c, d) True)
   in env2
pickChild env _ _ = env

dropChild :: [Elements] -> Elements -> Elements -> [Elements]
dropChild env (Robot (a, b) True) (Child (c, d) True) =
  let env1 = updateCell env (Robot (a, b) True) (Robot (a, b) False)
      env2 = updateCell env1 (Child (c, d) True) (Child (c, d) False)
   in env2
dropChild env _ _ = env

cleanDirt :: [Elements] -> Elements -> Elements -> [Elements]
cleanDirt env (Robot (a, b) False) (Dirty (c, d)) = removeCell (Dirty (c, d)) env False
cleanDirt env _ _ = env

newMatrix :: Int -> Int -> Matrix Int
newMatrix n m = matrix n m $ \(i, j) -> (-1)

gimmePositions :: [(Int, Int)] -> Matrix Int -> [(Int, Int)]
gimmePositions oldPos vMatrix = do
  (x, y) <- oldPos
  (a, b) <- [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]
  if boundsEnv (nrows vMatrix) (ncols vMatrix) a b && vMatrix ! (a, b) == -1 && notElem (a, b) newPos
    then newPos ++ [(a, b)]
    else newPos
  where
    newPos = []

bfs :: Int -> Int -> [Elements] -> (Int, Int) -> Matrix Int
bfs n m env (r, c) =
  let iMatrix = newMatrix n m
      vMatrix = setElem 0 (r, c) iMatrix
      iPos = gimmePositions [(r, c)] vMatrix
   in bfs1 env vMatrix iPos 1

bfs1 :: [Elements] -> Matrix Int -> [(Int, Int)] -> Int -> Matrix Int
bfs1 env vMatrix [] _ = vMatrix
bfs1 env vMatrix pos width =
  let (vPos, nvMatrix) = bfsHelper env vMatrix pos width []
      nPos = gimmePositions vPos nvMatrix
   in bfs1 env nvMatrix nPos (width + 1)

bfsHelper :: [Elements] -> Matrix Int -> [(Int, Int)] -> Int -> [(Int, Int)] -> ([(Int, Int)], Matrix Int)
bfsHelper _ vMatrix [] _ vPos = (vPos, vMatrix)
bfsHelper env vMatrix ((a, b) : pos) width vPos =
  let cell = index env (a, b)
      (nvMatrix, boolean) = case cell of
        [] -> (setElem width (a, b) vMatrix, True)
        [Dirty (a, b)] -> (setElem width (a, b) vMatrix, True)
        [Child (a, b) False] -> (setElem width (a, b) vMatrix, True)
        [Playpen (a, b)] -> (setElem width (a, b) vMatrix, True)
        _ -> (setElem (-2) (a, b) vMatrix, False)
   in if boolean then bfsHelper env nvMatrix pos width (vPos ++ [(a, b)]) else bfsHelper env nvMatrix pos width vPos

findNearestChild :: [Elements] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
findNearestChild [] _ _ (r, c) = (r, c)
findNearestChild (Child (a, b) False : children) vMatrix minW (r, c) =
  let dist = vMatrix ! (a, b)
   in if dist > 0 && dist < minW
        then findNearestChild children vMatrix dist (a, b)
        else findNearestChild children vMatrix minW (r, c)

findNearestPlaypen :: [Elements] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
findNearestPlaypen [] _ _ (r, c) = (r, c)
findNearestPlaypen (Playpen (a, b) : pp) vMatrix minW (r, c) =
  let dist = vMatrix ! (a, b)
   in if dist > 0 && dist < minW
        then findNearestPlaypen pp vMatrix dist (a, b)
        else findNearestPlaypen pp vMatrix minW (r, c)

findNearestDirty :: [Elements] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
findNearestDirty [] _ _ (r, c) = (r, c)
findNearestDirty (Dirty (a, b) : dd) vMatrix minW (r, c) =
  let dist = vMatrix ! (a, b)
   in if dist > 0 && dist < minW
        then findNearestDirty dd vMatrix dist (a, b)
        else findNearestDirty dd vMatrix minW (r, c)

minNeighbor :: [(Int, Int)] -> Matrix Int -> Int -> (Int, Int) -> (Int, Int)
minNeighbor [] _ _ (r, c) = (r, c)
minNeighbor ((a, b) : pos) vMatrix minW (r, c) =
  if boundsEnv (nrows vMatrix) (ncols vMatrix) a b
    then
      let dist = vMatrix ! (a, b)
       in if dist > -1 && dist < minW
            then minNeighbor pos vMatrix dist (a, b)
            else minNeighbor pos vMatrix minW (r, c)
    else minNeighbor pos vMatrix minW (r, c)

buildPath :: (Int, Int) -> Matrix Int -> Int -> (Int, Int)
buildPath (a, b) _ 1 = (a, b)
buildPath (a, b) vMatrix d =
  let (r, c) = minNeighbor [(a + 1, b), (a -1, b), (a, b + 1), (a, b -1)] vMatrix d (a, b)
      nDist = vMatrix ! (r, c)
   in buildPath (r, c) vMatrix nDist

buildPath2 :: (Int, Int) -> Matrix Int -> Int -> (Int, Int)
buildPath2 (a, b) _ 1 = (a, b)
buildPath2 (a, b) _ 2 = (a, b)
buildPath2 (a, b) vMatrix d =
  let (r, c) = minNeighbor [(a + 1, b), (a -1, b), (a, b + 1), (a, b -1)] vMatrix d (a, b)
      nDist = vMatrix ! (r, c)
   in buildPath2 (r, c) vMatrix nDist

getPlaypen :: [Elements] -> [Elements] -> [Elements] -> [Elements]
getPlaypen envO [] cenv = cenv
getPlaypen envO (Playpen (a, b) : env) cenv =
  if let envO1 = removeCell (Playpen (a, b)) envO False
      in not (taken envO1 (a, b))
    then
      let cenv1 = cenv ++ [Playpen (a, b)]
       in getPlaypen envO env cenv1
    else getPlaypen envO env cenv
getPlaypen envO (_ : env) cenv = getPlaypen envO env cenv

getRobots :: [Elements] -> [Elements] -> [Elements]
getRobots [] cenv = cenv
getRobots (Robot (a, b) c : env) cenv = let cenv1 = cenv ++ [Robot (a, b) c] in getRobots env cenv1
getRobots (_ : env) cenv = getRobots env cenv

getDirty :: [Elements] -> [Elements] -> [Elements]
getDirty [] cenv = cenv
getDirty (Dirty (a, b) : env) cenv = let cenv1 = cenv ++ [Dirty (a, b)] in getDirty env cenv1
getDirty (_ : env) cenv = getDirty env cenv