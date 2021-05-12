-- =======================================
-- Evidența angajaților firmei Generic SRL
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module Main where

import           ASCIIArt
import           Control.Exception
import           DataReader
import           EmployeeTable
import           Entities
import           GenericTable
import           System.Console.ANSI
import           System.IO

afisareAngajati :: () -> IO ()
afisareAngajati () = do
  clearScreen
  let spatiuStanga = 5
  let angajati     = citireAngajati ()
  printTable spatiuStanga angajati
  setCursorColumn (spatiuStanga + 1)
  putStrLn "Mai multe informații"
  setCursorColumn (spatiuStanga + 1)
  putStr "Număr matricol > "
  hFlush stdout
  matricol <- getLine
  clearScreen

meniuPrincipal :: () -> IO ()
meniuPrincipal () = do
  let titlu1       = "Evidența angajaților "
  let titlu2       = "firmei Generic SRL"
  let lungimeMeniu = 65
  let spatiuStanga = 5
  setTitle $ titlu1 ++ titlu2
  setCursorPosition 1 0
  printSplashScreen ()
  printMessage 13 spatiuStanga titlu2 lungimeMeniu
  setCursorPosition 15 spatiuStanga
  putStrLn "1) Afișare angajați"
  setCursorColumn spatiuStanga
  putStrLn "2) Editare informații angajat"
  setCursorColumn spatiuStanga
  putStrLn $ repeatChar '━' lungimeMeniu
  setCursorColumn spatiuStanga
  putStr "Opțiunea 1/2 > "
  hFlush stdout
  optiune <- getLine
  executare optiune

executare :: String -> IO ()
executare optiune
  | (optiune == "1") = do
    afisareAngajati ()
    meniuPrincipal ()
  | otherwise = do
    clearScreen
    printGoodbyeScreen ()

main :: IO ()
main = do
  clearScreen
  meniuPrincipal ()
