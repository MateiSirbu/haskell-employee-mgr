-- Evidența angajaților firmei Generic SRL
-- Sîrbu Matei-Dan, grupa 10LF383
-- http://github.msirbu.eu

module Main where

import           ReadData
import           Control.Exception
import           System.Console.ANSI
import           System.Directory               ( createDirectoryIfMissing )
import           System.FilePath.Posix          ( takeDirectory )
import           System.IO
import           System.IO.Error

afisareAngajati :: () -> IO ()
afisareAngajati () = do
  clearScreen
  setCursorPosition 0 10
  putStrLn "Afișare angajați"
  _ <- citireAngajati ()
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
  putStrLn "------------------------------"
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
