-- ==========================================
-- Afișare tabelară formatată
--
-- Prof. Dorin BOCU
-- FMI, Universitatea Transilvania din Brașov
-- 
-- Deobfuscat și modificat de Matei Sîrbu
-- ==========================================

module GenericTable where

import           Entities
import           System.Console.ANSI
import           System.IO

-- Subliniere pe linie specificata de la coloana de start specificata
underline :: Maybe Int -> Int -> Int -> Char -> IO ()

underline (Just ln) col length chr = do
  setCursorPosition ln col
  putStr (repeatChar chr length)
  hFlush stdout

underline Nothing col length chr = do
  setCursorColumn col
  putStr (repeatChar chr length)
  hFlush stdout

-- Replicare caracter de un numar de ori specificat
repeatChar :: Char -> Int -> String
repeatChar _   0  = []
repeatChar chr nr = chr : (repeatChar chr (nr - 1))

-- Determinare lungime subliniere aferenta afisare pe ecran
computeTableLength :: [(Int, Int)] -> IO Int
computeTableLength colSpecs = do
  noColumns        <- return (length colSpecs)
  totalLengthCells <- return (computeTotalCellLengths colSpecs)
  return (noColumns + totalLengthCells)

-- Determinare lungime exclusiv zone de afisare coloane inregistrare
computeTotalCellLengths :: [(Int, Int)] -> Int
computeTotalCellLengths []                           = 0
computeTotalCellLengths ((cellLength, _) : colSpecs) = cellLength + (computeTotalCellLengths colSpecs)

-- Afisare lista de inregistrari
printListOfStringLists :: Int -> [[String]] -> [(Int, Int)] -> IO ()
printListOfStringLists _  []             _        = return ()
printListOfStringLists cs (strL : lStrL) colSpecs = do
  printStringList cs strL colSpecs
  printListOfStringLists cs lStrL colSpecs

-- Afisare inregistrare
printStringList :: Int -> [String] -> [(Int, Int)] -> IO ()

printStringList spatiuStanga [str] [(lungimeCelula, aliniere)] = do
  printString spatiuStanga str lungimeCelula aliniere
  Just (ln, _) <- getCursorPosition
  col          <- return (spatiuStanga + lungimeCelula + 1)
  setCursorPosition ln col
  putStr "║"
  hFlush stdout
  putStrLn ""

printStringList spatiuStanga (str : lstStr) ((lungimeCelula, aliniere) : lstConfig) = do
  printString spatiuStanga str lungimeCelula aliniere
  printStringList (spatiuStanga + lungimeCelula + 1) lstStr lstConfig

-- Afisare coloana
printString :: Int -> String -> Int -> Int -> IO ()

-- Afisare cu aliniere la stanga
printString col str cellLength 0 = do
  Just (ln, _) <- getCursorPosition
  setCursorPosition ln col
  putStr "║"
  hFlush stdout
  putStr str
  hFlush stdout
  putStr (repeatChar ' ' (cellLength - (length str)))
  hFlush stdout

-- Afisare cu aliniere la dreapta
printString col str cellLength 1 = do
  Just (ln, _) <- getCursorPosition
  putStr "║"
  hFlush stdout
  depl <- return (cellLength - (length str))
  putStr (repeatChar ' ' depl)
  hFlush stdout
  setCursorPosition ln (col + depl + 1)
  putStr str
  hFlush stdout

-- Afisare centrata mesaj, pozitionata si cu lungime zona de afisare specificata
printMessage :: Int -> Int -> String -> Int -> IO ()
printMessage ln spatiuStanga msg tblLength = do
  depl <- return ((tblLength - (length msg)) `div` 2)
  col  <- return (spatiuStanga + depl + 1)
  setCursorPosition ln col
  putStr msg
  hFlush stdout
  putStr (repeatChar ' ' (tblLength - (length msg)))
  hFlush stdout
