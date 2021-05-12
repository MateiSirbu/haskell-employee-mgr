-- ==========================================
-- Afișare tabelară formatată
--
-- Prof. Dorin BOCU
-- FMI, Universitatea Transilvania din Brașov
-- 
-- Deobfuscat și modificat de Matei Sîrbu
-- ==========================================

module GenericTablePrinter where

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
repeatChar car nr = car : (repeatChar car (nr - 1))

-- Determinare lungime subliniere aferenta afisare pe ecran

computeTableLength :: [(Int, Int)] -> IO Int
computeTableLength colSpecs = do
  nb    <- return (length colSpecs)
  ltzaf <- return (computeTotalCellLengths colSpecs)
  return (nb + ltzaf)

-- Determinare lungime exclusiv zone de afisare coloane inregistrare

computeTotalCellLengths :: [(Int, Int)] -> Int
computeTotalCellLengths []                           = 0
computeTotalCellLengths ((cellLength, _) : colSpecs) = cellLength + (computeTotalCellLengths colSpecs)

-- Generare raport cu date furnizate sub forma de lista de inregistrari

generateReport :: Int -> [String] -> [[String]] -> [(Int, Int)] -> IO ()
generateReport cs dantet lInr ldf = do
  lza  <- (computeTableLength ldf)
  ltza <- return (lza + 1)
  printListOfStringLists cs lInr ldf

-- Subliniere finala

  Just (lc, _) <- getCursorPosition
  setCursorPosition lc cs
  underline (Just lc) cs ltza '-'

-- Afisare lista de inregistrari

printListOfStringLists :: Int -> [[String]] -> [(Int, Int)] -> IO ()
printListOfStringLists _  []             _        = return ()
printListOfStringLists cs (strL : lStrL) colSpecs = do
  printStringList cs strL colSpecs
  printListOfStringLists cs lStrL colSpecs

-- Afisare inregistrare

printStringList :: Int -> [String] -> [(Int, Int)] -> IO ()
printStringList cs [col] [(lza, cal)] = do
  printString cs col lza cal
  Just (lc, _) <- getCursorPosition
  cbv          <- return (cs + lza + 1)
  setCursorPosition lc cbv
  putStr "│"
  hFlush stdout
  putStrLn ""
printStringList cs (col : rlc) ((lza, cal) : rldf) = do
  printString cs col lza cal
  printStringList (cs + lza + 1) rlc rldf

-- Afisare coloana

printString :: Int -> String -> Int -> Int -> IO ()
-- Afisare cu aliniere la stanga
printString col str cellLength 0 = do
  Just (ln, _) <- getCursorPosition
  setCursorPosition ln col
  putStr "│"
  hFlush stdout
  putStr str
  hFlush stdout
  putStr (repeatChar ' ' (cellLength - (length str)))
  hFlush stdout
-- Afisare cu aliniere la dreapta
printString col str cellLength 1 = do
  Just (ln, _) <- getCursorPosition
  putStr "│"
  hFlush stdout
  depl <- return (cellLength - (length str))
  putStr (repeatChar ' ' depl)
  hFlush stdout
  setCursorPosition ln (col + depl + 1)
  putStr str
  hFlush stdout

-- Afisare centrata mesaj, pozitionata si cu lungime zona de afisare specificata

printMessage :: Int -> Int -> String -> Int -> IO ()
printMessage lc cs mes lza = do
  depl <- return ((lza - (length mes)) `div` 2)
  css  <- return (cs + depl + 1)
  setCursorPosition lc css
  putStr mes
  hFlush stdout
  putStr (repeatChar ' ' (lza - (length mes)))
  hFlush stdout
