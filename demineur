import System.Random (randomRIO)
import Data.Char (toUpper)
import Control.Monad (when, foldM)
import System.IO (hFlush, stdout)

-- ANSI colors
gray, red, green, yellow, blue, reset :: String
gray = "\x1b[90m"
red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
blue = "\x1b[34m"
reset = "\x1b[0m"

--  Paramètres du jeu
width, height, mineCount :: Int
width = 8
height = 8
mineCount = 10

--  Représentation d'une case
data Cell = Hidden Bool       -- Bool : True = mine
          | Revealed Bool     -- Bool : True = mine
          | Flagged Bool      -- Bool : True = mine
          deriving (Eq)

type Board = [[Cell]]

--  Mélange une liste
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (before, x:after) = splitAt i xs
    rest <- shuffle (before ++ after)
    return (x : rest)

--  Initialise un plateau avec des mines aléatoires
initializeBoard :: IO Board
initializeBoard = do
    let total = width * height
    indices <- shuffle [0..total-1]
    let mineIndices = take mineCount indices
    return [ [ Hidden ((r * width + c) `elem` mineIndices) | c <- [0..width-1] ] | r <- [0..height-1] ]

--  Affiche le plateau avec couleurs et indices
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn $ "   " ++ concatMap (\i -> show (i `mod` 10) ++ " ") [0..width-1]
    putStrLn $ "  +" ++ replicate (width * 2 - 1) '-'
    mapM_ printRow [0..height-1]
  where
    printRow r = do
        putStr $ show (r `mod` 10) ++ " |"
        mapM_ (\c -> putStr $ displayCell (board !! r !! c) ++ " ") [0..width-1]
        putStrLn ""
    displayCell (Hidden _)     = gray ++ "?" ++ reset
    displayCell (Flagged _)    = yellow ++ "⚑" ++ reset
    displayCell (Revealed True)  = red ++ "💣" ++ reset
    displayCell (Revealed False) = green ++ "." ++ reset

--  Modifie une case du plateau
setCell :: Board -> Int -> Int -> Cell -> Board
setCell b r c cell =
    take r b ++ [take c (b !! r) ++ [cell] ++ drop (c + 1) (b !! r)] ++ drop (r + 1) b

--  Compte les mines autour d'une case
countAdjacentMines :: Board -> Int -> Int -> Int
countAdjacentMines board row col =
    length [ () | r <- [max 0 (row-1)..min (height-1) (row+1)],
                  c <- [max 0 (col-1)..min (width-1) (col+1)],
                  (r /= row || c /= col),
                  isMine (board !! r !! c) ]
  where
    isMine (Hidden m) = m
    isMine (Flagged m) = m
    isMine _ = False

--  Révèle une case
revealCell :: Board -> Int -> Int -> IO (Board, Bool)
revealCell board row col
    | row < 0 || row >= height || col < 0 || col >= width = return (board, False)
    | otherwise = case board !! row !! col of
        Hidden True -> return (setCell board row col (Revealed True), True)
        Hidden False -> do
            let b' = setCell board row col (Revealed False)
            let mines = countAdjacentMines b' row col
            if mines == 0 then
                foldM (\(acc, _) (r, c) -> revealCell acc r c)
                      (b', False)
                      [ (r, c) |
                        r <- [max 0 (row-1)..min (height-1) (row+1)],
                        c <- [max 0 (col-1)..min (width-1) (col+1)],
                        (r /= row || c /= col) ]
            else return (b', False)
        _ -> return (board, False)

--  Vérifie si le joueur a gagné
hasWon :: Board -> Bool
hasWon board = all check (concat board)
  where
    check (Hidden False) = False
    check (Flagged False) = False
    check _ = True

--  Bascule le drapeau sur une case
toggleFlag :: Board -> Int -> Int -> Board
toggleFlag b r c
    | r < 0 || r >= height || c < 0 || c >= width = b
    | otherwise = setCell b r c $ case b !! r !! c of
        Hidden m  -> Flagged m
        Flagged m -> Hidden m
        x         -> x

--  Boucle du jeu
gameLoop :: Board -> Int -> IO ()
gameLoop board moves = do
    putStrLn $ blue ++ "\nMouvements : " ++ show moves ++ reset
    displayBoard board
    putStr "Commande (r row col = révéler | f row col = drapeau | q = quitter) : "
    hFlush stdout
    input <- getLine
    case words input of
        ["q"] -> putStrLn "👋 Au revoir !"
        ["r", rStr, cStr] ->
            let r = read rStr
                c = read cStr
            in do
                (b', exploded) <- revealCell board r c
                if exploded then do
                    displayBoard b'
                    putStrLn $ red ++ "\n💥 BOOM ! Vous avez perdu.\n" ++ reset
                else if hasWon b' then do
                    displayBoard b'
                    putStrLn $ green ++ "\n🎉 Bravo ! Vous avez gagné en " ++ show (moves + 1) ++ " coups !" ++ reset
                else gameLoop b' (moves + 1)
        ["f", rStr, cStr] ->
            let r = read rStr
                c = read cStr
            in gameLoop (toggleFlag board r c) (moves + 1)
        _ -> do
            putStrLn $ red ++ "❌ Commande invalide. Essayez encore." ++ reset
            gameLoop board moves

--  Lancement
main :: IO ()
main = do
    putStrLn $ blue ++ "=== 🧨 DÉMINEUR TERMINAL ===" ++ reset
    putStrLn $ "Plateau " ++ show width ++ "×" ++ show height ++ " | Mines : " ++ show mineCount
    board <- initializeBoard
    gameLoop board 0
