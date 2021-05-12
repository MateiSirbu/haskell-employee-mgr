-- =======================================
-- Afișare tabelară angajați
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module EmployeeTablePrinter where

import           Control.Monad
import           Entities
import           GenericTablePrinter
import           System.Console.ANSI


printTable :: IO [Angajat] -> IO ()
printTable angajati = do
  let offset   = 5
  let colSpecs = [(10, 0), (20, 0), (20, 0)]
  printHeader offset colSpecs ["Matricol", "Nume", "Prenume"]
  printRows offset colSpecs angajati

employeeToStringList :: Angajat -> [String]
employeeToStringList z = do
  let matr      = show $ matricol (datePersonale $ z)
  let numele    = nume (datePersonale z)
  let prenumele = prenume (datePersonale z)
  result <- [matr, numele, prenumele]
  return result

printRows :: Int -> [(Int, Int)] -> IO [Angajat] -> IO ()
printRows offset colSpecs ioAngajati = do
  angajati <- ioAngajati
  tblLen <- computeTableLength colSpecs
  let listaAngajati = map employeeToStringList angajati
  printListOfStringLists offset listaAngajati colSpecs
  underline Nothing (offset + 1) (tblLen - 1) '━'

printHeader :: Int -> [(Int, Int)] -> [String] -> IO ()
printHeader offset colSpecs antet = do
  tblLen <- computeTableLength colSpecs
  printMessage 0 offset "Afișare angajați" tblLen
  setCursorPosition 1 offset
  putStrLn "┌"
  underline (Just 1) (offset + 1) (tblLen - 1) '━'
  putStrLn "┐"
  setCursorPosition 2 offset
  printStringList offset antet colSpecs
  setCursorPosition 3 offset
  putStrLn "└"
  underline (Just 3) (offset + 1) (tblLen - 1) '━'
  putStrLn "┘"
