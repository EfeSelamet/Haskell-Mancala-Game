--150122058 Mehmet Efe Selamet
--150121016 Askin Yavuz Tuna
--150121521 Emre Kursun
import System.Exit

printGameBoard :: [Int] -> IO ()
printGameBoard holes = do
  let blank = if holes !! 13 > 9 then " " else ""
  putStrLn "\n         GAME MANGALA\n"
  putStrLn $ show (holes !! 13) ++ "  | " ++ show (holes !! 12) ++ " | " ++ show (holes !! 11) ++ " | " ++ show (holes !! 10) ++ " | " ++ show (holes !! 9) ++ " | " ++ show (holes !! 8) ++ " | " ++ show (holes !! 7) ++ " |"
  putStrLn $ blank ++ "   | " ++ show (holes !! 0) ++ " | " ++ show (holes !! 1) ++ " | " ++ show (holes !! 2) ++ " | " ++ show (holes !! 3) ++ " | " ++ show (holes !! 4) ++ " | " ++ show (holes !! 5) ++ " |  " ++ show (holes !! 6)

run :: String -> [Int] -> IO ()
run player holes = do

  let gameOver =
        if ((holes !! 0) == 0 && (holes !! 1) == 0 && (holes !! 2) == 0 && (holes !! 3) == 0 && (holes !! 4) == 0 && (holes !! 5) == 0)
          || ((holes !! 7) == 0 && (holes !! 8) == 0 && (holes !! 9) == 0 && (holes !! 10) == 0 && (holes !! 11) == 0 && (holes !! 12) == 0)
          then 1
          else 0

  let takeStones =
        if (gameOver == 1) && (player == "2")
          then (holes !! 7) + (holes !! 8) + (holes !! 9) + (holes !! 10) + (holes !! 11) + (holes !! 12)
          else (holes !! 0) + (holes !! 1) + (holes !! 2) + (holes !! 3) + (holes !! 4) + (holes !! 5)


  let (player1Stones, player2Stones) =
        if (gameOver == 1) && (player == "2")
          then ((holes !! 6) + takeStones, holes !! 13)
          else (holes !! 6, (holes !! 13) + takeStones)

  let winner = if(player1Stones > player2Stones)
                then "Player 1"
                else if(player1Stones == player2Stones)
                      then "Draw"
                      else "Player 2"

  if(gameOver == 0)
    then( do
      putStrLn $ "\nPlayer " ++ show player ++ " turn"
      putStrLn ("Player " ++ player ++ " Enter hole number: ")
      hole <- getLine
      let holeIndex' = (read hole :: Int) - 1
      let holeIndex = if player == "2" then holeIndex' + 7 else holeIndex'
      let stones = holes !! holeIndex
      let newHoles = take holeIndex holes ++ [1] ++ drop (holeIndex + 1) holes
      let leaveOneStoneForCurrentHole = if stones == 1 then 0 else 1
      let lastFilledHoleNumber = if stones + holeIndex' >= 14 then (holeIndex + stones - leaveOneStoneForCurrentHole + 2) `mod` 14 else (holeIndex + stones - leaveOneStoneForCurrentHole + 1) `mod` 14



      let (newHoles', lastHole) = distributeStones newHoles holeIndex (stones - leaveOneStoneForCurrentHole) player
      let newHoles'' = take holeIndex newHoles' ++ [leaveOneStoneForCurrentHole] ++ drop (holeIndex + 1) newHoles'
      let newHoles''' = take 14 newHoles'' ++ [lastHole] ++ drop 15 newHoles''
      let isSteal = if player == "1" then lastFilledHoleNumber < 7 else lastFilledHoleNumber > 6
      
      if isSteal && (lastFilledHoleNumber /= 7) && (lastFilledHoleNumber /= 0) && (newHoles''' !! (lastFilledHoleNumber - 1)) == 1
        then steal (lastFilledHoleNumber - 1) player newHoles'''
        else
          putStr ""

      printGameBoard newHoles'''
  
      if lastFilledHoleNumber `mod` 7 == 0
        then run player newHoles'''
        else
          if player == "1"
            then run "2" newHoles'''
            else run "1" newHoles''')
    else
      die ("Game Over" ++ "\n" ++ "Player 1 stones: " ++ show player1Stones ++ "\nPlayer 2 stones: " ++ show player2Stones ++ "\nThe winner is: " ++ winner)

distributeStones :: [Int] -> Int -> Int -> String -> ([Int], Int)
distributeStones holes holeIndex stones player
  | stones == 0 = (holes, 0)
  | otherwise = distributeStones newHoles newHoleIndex newStones player
  where
    newHoleIndex' = (holeIndex + 1) `mod` 14
    newHoleIndex =
      if (player == "1" && newHoleIndex' == 13) || (player == "2" && newHoleIndex' == 6)
        then (newHoleIndex' + 1) `mod` 14
        else newHoleIndex'
    newStones = stones - 1
    newHoles = take newHoleIndex holes ++ [holes !! newHoleIndex + 1] ++ drop (newHoleIndex + 1) holes

steal :: Int -> String -> [Int] -> IO ()
steal lastFilledIndex player newHoles = do
  let lastFilledOppositeIndex = 12 - lastFilledIndex
  let boxIndex = if player == "1" then 6 else 13
  let totalSteal = (newHoles !! lastFilledIndex) + (newHoles !! lastFilledOppositeIndex)
  let newBox = (newHoles !! boxIndex) + totalSteal
  let newHoles' = take lastFilledIndex newHoles ++ [0] ++ drop (lastFilledIndex + 1) newHoles
  let newHoles'' = take boxIndex newHoles' ++ [newBox] ++ drop (boxIndex + 1) newHoles'
  let newHoles''' = take lastFilledOppositeIndex newHoles'' ++ [0] ++ drop (lastFilledOppositeIndex + 1) newHoles''

  printGameBoard newHoles'''
  if player == "1"
    then
      run "2" newHoles'''
    else
      run "1" newHoles'''

main :: IO ()
main = do
  let holes = [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0]
  putStrLn "Select player 1 or 2: "
  player <- getLine
  printGameBoard holes
  run player holes