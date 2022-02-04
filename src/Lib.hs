module Lib
  ( sayHello,
    boardPrint,
    run,
  )
where

import Agents
import Data.Matrix
import EnvChange
import EnvElements
import Environment

sayHello :: String -> IO ()
sayHello x = simulation 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 4) False, Child (3, 2) False, Robot (1, 2) False, Obstacle (3, 4), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1), Dirty (1, 1), Dirty (1, 3), Dirty (1, 5), Dirty (2, 1), Dirty (2, 3), Dirty (2, 4), Dirty (2, 5), Dirty (4, 3)] True False 0 1

run :: IO ()
run = do
  testReactAgent
  testProactAgent

testReactAgent :: IO ()
testReactAgent = do
  let n = 4
      m = 5
      total = fromIntegral (n * m)
      dirt = round ((50 * total) / 100)
      obstacles = 3
      robot = 1
      children = 2
      env = initEnv n m robot children obstacles dirt
   in simulation n m env True False 0 1

  let n = 9
      m = 10
      total = fromIntegral (n * m)
      dirt = round ((50 * total) / 100)
      obstacles = 7
      robot = 2
      children = 4
      env = initEnv n m robot children obstacles dirt
   in simulation n m env True False 0 1

  let n = 11
      m = 11
      total = fromIntegral (n * m)
      dirt = round ((50 * total) / 100)
      obstacles = 15
      robot = 3
      children = 7
      env = initEnv n m robot children obstacles dirt
   in simulation n m env True False 0 1

testProactAgent :: IO ()
testProactAgent = do
  let n = 4
      m = 5
      total = fromIntegral (n * m)
      dirt = round ((50 * total) / 100)
      obstacles = 3
      robot = 1
      children = 2
      env = initEnv n m robot children obstacles dirt
   in simulation' n m env True False 0 1

  let n = 9
      m = 10
      total = fromIntegral (n * m)
      dirt = round ((50 * total) / 100)
      obstacles = 7
      robot = 2
      children = 4
      env = initEnv n m robot children obstacles dirt
   in simulation' n m env True False 0 1

  let n = 11
      m = 11
      total = fromIntegral (n * m)
      dirt = round ((50 * total) / 100)
      obstacles = 15
      robot = 3
      children = 7
      env = initEnv n m robot children obstacles dirt
   in simulation' n m env True False 0 1

simulation :: Int -> Int -> [Elements] -> Bool -> Bool -> Int -> Int -> IO ()
simulation _ _ env _ True _ _ = putStrLn "End"
simulation n m env cBool endBool time t = do
  let iMatrix = matrix n m $ \(i, j) -> "---"
      vMatrix = boardPrint env iMatrix
      countDirt = getDirty env []
      total = fromIntegral (n * m)
      part = fromIntegral (length countDirt)
      dirtPercentage = round ((part / total) * 100)
      cleanPercentage = 100 - dirtPercentage
   in printState n m env cBool time vMatrix (show cleanPercentage)

  let countDirt = getDirty env []
      total = fromIntegral (n * m)
      part = fromIntegral (length countDirt)
      dirtPercentage = round ((part / total) * 100)
      nt = time + 1
      ncBool = (nt `mod` t == 0)
   in if dirtPercentage < 40
        then simulation n m env cBool True nt t
        else
          if cBool
            then
              let env1 = robotAction n m env
                  env2 = childrenAction n m env1
               in simulation n m env2 ncBool False nt t
            else
              let env1 = robotAction n m env
               in simulation n m env1 ncBool False nt t

boardPrint :: [Elements] -> Matrix String -> Matrix String
boardPrint [] sMatrix = sMatrix
boardPrint (Dirty (a, b) : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "D" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "D"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Obstacle (a, b) : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "O" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "O"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Playpen (a, b) : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "P" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "P"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Child (a, b) c : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "C" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "C"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Robot (a, b) c : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "R" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "R"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix

printState :: Int -> Int -> [Elements] -> Bool -> Int -> Matrix String -> String -> IO ()
printState n m env cBool time vMatrix clean = do
  print "---------------------- Robot de Casa ----------------------------"
  print ("----------Agente Reactivo ------ Tiempo transcurrido :" ++ show time ++ "-----Limpio : " ++ clean ++ "%")
  print ("Filas: " ++ show n ++ " Columnas: " ++ show m)
  print ("Variacion Aleatoria de Ambiente " ++ show cBool)
  print vMatrix

simulation' :: Int -> Int -> [Elements] -> Bool -> Bool -> Int -> Int -> IO ()
simulation' _ _ env _ True _ _ = putStrLn "End"
simulation' n m env cBool endBool time t = do
  let iMatrix = matrix n m $ \(i, j) -> "---"
      vMatrix = boardPrint env iMatrix
      countDirt = getDirty env []
      total = fromIntegral (n * m)
      part = fromIntegral (length countDirt)
      dirtPercentage = round ((part / total) * 100)
      cleanPercentage = 100 - dirtPercentage
   in printState' n m env cBool time vMatrix (show cleanPercentage)

  let countDirt = getDirty env []
      total = fromIntegral (n * m)
      part = fromIntegral (length countDirt)
      dirtPercentage = round ((part / total) * 100)
      nt = time + 1
      ncBool = (nt `mod` t == 0)
   in if dirtPercentage < 40
        then simulation' n m env cBool True nt t
        else
          if cBool
            then
              let env1 = robotAction' n m env
                  env2 = childrenAction n m env1
               in simulation' n m env2 ncBool False nt t
            else
              let env1 = robotAction' n m env
               in simulation' n m env1 ncBool False nt t

printState' :: Int -> Int -> [Elements] -> Bool -> Int -> Matrix String -> String -> IO ()
printState' n m env cBool time vMatrix clean = do
  print "---------------------- Robot de Casa ----------------------------"
  print ("----------Agente Proactivo ------ Tiempo transcurrido :" ++ show time ++ "-----Limpio : " ++ clean ++ "%")
  print ("Filas: " ++ show n ++ " Columnas: " ++ show m)
  print ("Variacion Aleatoria de Ambiente " ++ show cBool)
  print vMatrix