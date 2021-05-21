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
import           EmployeePreview
import           EmployeeTable
import           EmployeeWriter
import           Entities
import           GenericTable
import           System.Console.ANSI
import           System.IO

afisareInformatiiAngajat :: Int -> Int -> [Angajat] -> IO ()
afisareInformatiiAngajat spatiuStanga lungimeEcran rezCautare = if length rezCautare == 0
  then do
    clearScreen
    printWarning 2 spatiuStanga lungimeEcran
    printMessage 9  spatiuStanga "EROARE"                          lungimeEcran
    printMessage 11 spatiuStanga "Nu s-a putut găsi un angajat"    lungimeEcran
    printMessage 12 spatiuStanga "cu numărul matricol specificat." lungimeEcran
    printMessage 14 spatiuStanga "[Apăsați ENTER] "                lungimeEcran
    _ <- getChar
    afisareAngajati spatiuStanga lungimeEcran
  else if length rezCautare == 1
    then do
      clearScreen
      let angajat = rezCautare !! 0
      afisareCompletaAngajat spatiuStanga angajat
      afisareAngajati spatiuStanga lungimeEcran
    else do
      clearScreen
      printWarning 2 spatiuStanga lungimeEcran
      printMessage 9  spatiuStanga "EROARE"                              lungimeEcran
      printMessage 11 spatiuStanga "Mai mulți angajați au același număr" lungimeEcran
      printMessage 12 spatiuStanga "matricol. Verificați integritatea fișierului." lungimeEcran
      printMessage 14 spatiuStanga "[Apăsați ENTER] "                    lungimeEcran
      _ <- getChar
      afisareAngajati spatiuStanga lungimeEcran


afisareAngajati :: Int -> Int -> IO ()
afisareAngajati spatiuStanga lungimeEcran = do
  clearScreen
  let ioAngajati = citireAngajati ()
  printTable spatiuStanga ioAngajati
  setCursorColumn (spatiuStanga + 1)
  putStrLn "Mai multe informații"
  setCursorColumn (spatiuStanga + 1)
  putStr "Număr matricol > "
  hFlush stdout
  matricol <- getLine
  angajati <- ioAngajati
  if matricol == mempty then clearScreen else afisareInformatiiAngajat spatiuStanga lungimeEcran $ cautareAngajat matricol angajati

adaugareAngajat :: Int -> Int -> IO ()
adaugareAngajat spatiuStanga lungimeEcran = do
  clearScreen
  let ioAngajatiExistenti = citireAngajati ()
  angajatiExistenti <- ioAngajatiExistenti
  angajatNou        <- citireAngajatFormular spatiuStanga lungimeEcran angajatiExistenti
  adaugareAngajatInFisier spatiuStanga angajatNou
  clearScreen

meniuPrincipal :: () -> IO ()
meniuPrincipal () = do
  let titlu1       = "Evidența angajaților "
  let titlu2       = "firmei Generic SRL"
  let lungimeEcran = 65
  let spatiuStanga = 5
  setTitle $ titlu1 ++ titlu2
  setCursorPosition 1 0
  printSplashScreen ()
  printMessage 13 spatiuStanga titlu2 lungimeEcran
  setCursorPosition 15 spatiuStanga
  putStrLn "1) Afișare angajați"
  setCursorColumn spatiuStanga
  putStrLn "2) Adăugare angajat"
  setCursorColumn spatiuStanga
  putStrLn $ repeatChar '━' lungimeEcran
  setCursorColumn spatiuStanga
  putStr "Opțiunea 1/2 > "
  hFlush stdout
  optiune <- getLine
  executare spatiuStanga lungimeEcran optiune

executare :: Int -> Int -> String -> IO ()
executare spatiuStanga lungimeEcran optiune
  | (optiune == "1") = do
    afisareAngajati spatiuStanga lungimeEcran
    meniuPrincipal ()
  | (optiune == "2") = do
    adaugareAngajat spatiuStanga lungimeEcran
    meniuPrincipal ()
  | (optiune == mempty) = do
    clearScreen
    printGoodbyeScreen ()
  | otherwise = do
    clearScreen
    printWarning 2 spatiuStanga lungimeEcran
    printMessage 9  spatiuStanga "EROARE"                               lungimeEcran
    printMessage 11 spatiuStanga "Opțiunea aleasă este invalidă."       lungimeEcran
    printMessage 12 spatiuStanga "Pentru ieșire alegeți opțiunea nulă." lungimeEcran
    printMessage 14 spatiuStanga "[Apăsați ENTER] "                     lungimeEcran
    _ <- getLine
    clearScreen
    meniuPrincipal ()

main :: IO ()
main = do
  clearScreen
  meniuPrincipal ()
