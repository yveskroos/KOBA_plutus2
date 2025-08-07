import System.Random (randomRIO)
import Data.Char (toUpper)
import Control.Monad (when, foldM)
import System.IO (hFlush, stdout)

-- Couleurs ANSI
gris, rouge, vert, jaune, bleu, reset :: String
gris = "\x1b[90m"
rouge = "\x1b[31m"
vert = "\x1b[32m"
jaune = "\x1b[33m"
bleu = "\x1b[34m"
reset = "\x1b[0m"

-- Paramètres du jeu
largeur, hauteur, nbMines :: Int
largeur = 8
hauteur = 8
nbMines = 10

-- Représentation d’une case
data Case = Couverte Bool       -- Bool : True = mine
          | Decouverte Bool     -- Bool : True = mine
          | Drapeau Bool        -- Bool : True = mine
          deriving (Eq)

type Plateau = [[Case]]

-- Mélange une liste
melanger :: [a] -> IO [a]
melanger [] = return []
melanger xs = do
    i <- randomRIO (0, length xs - 1)
    let (avant, x:apres) = splitAt i xs
    reste <- melanger (avant ++ apres)
    return (x : reste)

-- Initialise le plateau avec des mines aléatoires
initialiserPlateau :: IO Plateau
initialiserPlateau = do
    let total = largeur * hauteur
    indices <- melanger [0..total-1]
    let indicesMines = take nbMines indices
    return [ [ Couverte ((r * largeur + c) `elem` indicesMines) | c <- [0..largeur-1] ] | r <- [0..hauteur-1] ]

-- Affiche le plateau avec couleurs et indices
afficherPlateau :: Plateau -> IO ()
afficherPlateau plateau = do
    putStrLn $ "   " ++ concatMap (\i -> show (i `mod` 10) ++ " ") [0..largeur-1]
    putStrLn $ "  +" ++ replicate (largeur * 2 - 1) '-'
    mapM_ afficherLigne [0..hauteur-1]
  where
    afficherLigne r = do
        putStr $ show (r `mod` 10) ++ " |"
        mapM_ (\c -> putStr $ afficherCase (plateau !! r !! c) ++ " ") [0..largeur-1]
        putStrLn ""
    afficherCase (Couverte _)       = gris ++ "?" ++ reset
    afficherCase (Drapeau _)        = jaune ++ "⚑" ++ reset
    afficherCase (Decouverte True)  = rouge ++ "💣" ++ reset
    afficherCase (Decouverte False) = vert ++ "." ++ reset

-- Modifie une case du plateau
modifierCase :: Plateau -> Int -> Int -> Case -> Plateau
modifierCase p r c casee =
    take r p ++ [take c (p !! r) ++ [casee] ++ drop (c + 1) (p !! r)] ++ drop (r + 1) p

-- Compte les mines autour d’une case
compterMinesAdjacentes :: Plateau -> Int -> Int -> Int
compterMinesAdjacentes plateau ligne col =
    length [ () | r <- [max 0 (ligne-1)..min (hauteur-1) (ligne+1)],
                  c <- [max 0 (col-1)..min (largeur-1) (col+1)],
                  (r /= ligne || c /= col),
                  estMine (plateau !! r !! c) ]
  where
    estMine (Couverte m) = m
    estMine (Drapeau m)  = m
    estMine _            = False

-- Révèle une case
revelerCase :: Plateau -> Int -> Int -> IO (Plateau, Bool)
revelerCase plateau ligne col
    | ligne < 0 || ligne >= hauteur || col < 0 || col >= largeur = return (plateau, False)
    | otherwise = case plateau !! ligne !! col of
        Couverte True -> return (modifierCase plateau ligne col (Decouverte True), True)
        Couverte False -> do
            let p' = modifierCase plateau ligne col (Decouverte False)
            let mines = compterMinesAdjacentes p' ligne col
            if mines == 0 then
                foldM (\(acc, _) (r, c) -> revelerCase acc r c)
                      (p', False)
                      [ (r, c) |
                        r <- [max 0 (ligne-1)..min (hauteur-1) (ligne+1)],
                        c <- [max 0 (col-1)..min (largeur-1) (col+1)],
                        (r /= ligne || c /= col) ]
            else return (p', False)
        _ -> return (plateau, False)

-- Vérifie si le joueur a gagné
aGagne :: Plateau -> Bool
aGagne plateau = all verifie (concat plateau)
  where
    verifie (Couverte False) = False
    verifie (Drapeau False)  = False
    verifie _ = True

-- Met un drapeau ou le retire
basculerDrapeau :: Plateau -> Int -> Int -> Plateau
basculerDrapeau p r c
    | r < 0 || r >= hauteur || c < 0 || c >= largeur = p
    | otherwise = modifierCase p r c $ case p !! r !! c of
        Couverte m -> Drapeau m
        Drapeau m  -> Couverte m
        x          -> x

-- Boucle du jeu
boucleJeu :: Plateau -> Int -> IO ()
boucleJeu plateau coups = do
    putStrLn $ bleu ++ "\nNombre de coups : " ++ show coups ++ reset
    afficherPlateau plateau
    putStr "Commande (r ligne colonne = révéler | f ligne colonne = drapeau | q = quitter) : "
    hFlush stdout
    entree <- getLine
    case words entree of
        ["q"] -> putStrLn "👋 Au revoir !"
        ["r", lStr, cStr] ->
            let l = read lStr
                c = read cStr
            in do
                (p', explose) <- revelerCase plateau l c
                if explose then do
                    afficherPlateau p'
                    putStrLn $ rouge ++ "\n💥 BOOM ! Vous avez perdu.\n" ++ reset
                else if aGagne p' then do
                    afficherPlateau p'
                    putStrLn $ vert ++ "\n🎉 Bravo ! Vous avez gagné en " ++ show (coups + 1) ++ " coups !" ++ reset
                else boucleJeu p' (coups + 1)
        ["f", lStr, cStr] ->
            let l = read lStr
                c = read cStr
            in boucleJeu (basculerDrapeau plateau l c) (coups + 1)
        _ -> do
            putStrLn $ rouge ++ "❌ Commande invalide. Essayez encore." ++ reset
            boucleJeu plateau coups

-- Lancement du jeu
main :: IO ()
main = do
    putStrLn $ bleu ++ "=== 🧨 DÉMINEUR EN TERMINAL ===" ++ reset
    putStrLn $ "Plateau " ++ show largeur ++ "×" ++ show hauteur ++ " | Mines : " ++ show nbMines
    plateau <- initialiserPlateau
    boucleJeu plateau 0
