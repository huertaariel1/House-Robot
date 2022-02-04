module Lib
  ( sayHello,
    boardPrint,
  )
where

import Agents
import Data.Matrix
import EnvChange
import EnvElements
import Environment

sayHello :: String -> IO ()
sayHello x = let y = bfs 4 5 [Playpen (4, 4), Playpen (4, 5), Child (4, 5) True, Child (4, 4) False, Robot (1, 2) False, Obstacle (3, 3), Obstacle (3, 4), Obstacle (3, 2), Dirty (1, 4), Dirty (3, 5), Dirty (4, 1)] (1, 2) in print y

simulation :: Int -> Int -> [Elements] -> Bool -> Bool -> Int -> Int -> IO ()
simulation _ _ env _ True _ _ = putStrLn "End"
simulation n m env cBool endBool time t =
  let countDirt = getDirty env []
      total = fromIntegral (n * m)
      part = fromIntegral (length countDirt)
      dirtPercentage = round ((part / total) * 100)
      iMatrix = matrix n m $ \(i, j) -> "---"
      vMatrix = boardPrint env iMatrix
      meh = printState n m env cBool time vMatrix
      nt = time + 1
      ncBool = (nt `mod` t == 0)
   in if dirtPercentage < 40
        then simulation n m env cBool True nt t
        else
          if cBool
            then
              let env1 = childrenAction n m env
                  env2 = robotAction n m env1
               in simulation n m env2 ncBool False nt t
            else
              let env1 = robotAction n m env1
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

printState :: Int -> Int -> [Elements] -> Bool -> Int -> Matrix String -> IO ()
printState n m env cBool time vMatrix = do
  print "---------------------- Robot de Casa ----------------------------"
  print ("----------Agente Reactivo ------ Tiempo transcurrido :" ++ show time)
  print ("Filas:" ++ show n ++ "Columnas: " ++ show m)
  print ("Variación Aleatoria de Ambiente" ++ show cBool)
  print vMatrix
