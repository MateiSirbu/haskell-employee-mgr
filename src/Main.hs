-- =======================================
-- Evidența angajaților firmei Generic SRL
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module Main where

import           Control.Exception
import           EmployeeTablePrinter
import           Entities
import           GenericTablePrinter
import           ReadData
import           System.Console.ANSI
import           System.IO

afisareAngajati :: () -> IO ()
afisareAngajati () = do
  clearScreen
  setCursorPosition 0 10
  let angajati = citireAngajati ()
  printTable angajati
  _ <- getLine
  clearScreen

meniuPrincipal :: () -> IO ()
meniuPrincipal () = do
  let titlu = "Evidența angajaților firmei Generic SRL"
  setTitle titlu
  setCursorPosition 0 10
  putStrLn titlu
  putStrLn ""
  setCursorColumn 5
  putStrLn "1) Afișare angajați"
  setCursorColumn 5
  putStrLn "2) Editare informații angajat"
  setCursorColumn 5
  putStrLn $ repeatChar '━' 45
  setCursorColumn 5
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
    putStrLn "NANI?!"

main :: IO ()
main = do
  clearScreen
  meniuPrincipal ()
