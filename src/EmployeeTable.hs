-- =======================================
-- Afișare tabelară angajați
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module EmployeeTable
  ( printTable
  ) where

import           Control.Monad
import           Entities
import           GenericTable
import           System.Console.ANSI

printTable :: Int -> IO [Angajat] -> IO ()
printTable spatiuStanga angajati = do
  let colSpecs = [(8, 0), (25, 0), (25, 0), (25, 0)]
  printHeader spatiuStanga colSpecs ["MATRICOL", "NUME", "PRENUME", "ADRESĂ E-MAIL"]
  printRows spatiuStanga colSpecs angajati

employeeToStringList :: Angajat -> [String]
employeeToStringList z = do
  let campMatricol = show $ matricol $ datePersonale z
  let campNume     = nume $ datePersonale z
  let campPrenume  = prenume $ datePersonale z
  let campEmail    = email $ datePersonale z
  result <- [campMatricol, campNume, campPrenume, campEmail]
  return result

printRows :: Int -> [(Int, Int)] -> IO [Angajat] -> IO ()
printRows offset colSpecs ioAngajati = do
  angajati <- ioAngajati
  tblLen   <- computeTableLength colSpecs
  let listaAngajati = map employeeToStringList angajati
  printListOfStringLists offset listaAngajati colSpecs
  setCursorColumn offset
  putStr "╚"
  underline Nothing (offset + 1) (tblLen - 1) '═'
  putStrLn "╝"

printHeader :: Int -> [(Int, Int)] -> [String] -> IO ()
printHeader offset colSpecs antet = do
  tblLen <- computeTableLength colSpecs
  printMessage 1 offset "Afișare angajați" tblLen
  setCursorPosition 3 offset
  putStrLn "╔"
  underline (Just 3) (offset + 1) (tblLen - 1) '═'
  putStrLn "╗"
  setCursorPosition 4 offset
  printStringList offset antet colSpecs
  setCursorPosition 5 offset
  putStrLn "╠"
  underline (Just 5) (offset + 1) (tblLen - 1) '═'
  putStrLn "╣"
