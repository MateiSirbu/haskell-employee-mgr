-- =======================================
-- Afișare elaborată angajați
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module EmployeePreview where

import           ASCIIArt
import           EmployeeTable
import           Entities
import           GenericTable
import           System.Console.ANSI
import           System.IO

afisareRandInfoAngajat :: Int -> Int -> String -> String -> IO ()
afisareRandInfoAngajat spatiuCol1 spatiuCol2 col1 col2 = do
  setCursorColumn spatiuCol1
  putStr col1
  setCursorColumn spatiuCol2
  putStrLn col2

afisareDocumente :: Int -> [String] -> IO ()
afisareDocumente spatiuStanga [] = do
  setCursorColumn spatiuStanga
  putStrLn ""
  hFlush stdout
afisareDocumente spatiuStanga (cap : docs) = do
  setCursorColumn spatiuStanga
  putStrLn ("   -  " ++ cap)
  afisareDocumente spatiuStanga docs

afisareStudii :: Int -> [Studii] -> IO ()
afisareStudii spatiuStanga [] = do
  setCursorColumn spatiuStanga
  hFlush stdout
afisareStudii spatiuStanga (cap : studii) = do
  setCursorColumn spatiuStanga
  putStrLn ("-  " ++ perioadaStudii cap ++ ": " ++ institutie cap ++ " (" ++ tipStudii cap ++ ", " ++ specializare cap ++ ") ")
  setCursorColumn spatiuStanga
  putStrLn ("Documente (" ++ show (length (documenteAbsolvire cap)) ++ ")")
  afisareDocumente spatiuStanga (documenteAbsolvire cap)
  afisareStudii spatiuStanga studii

afisareIstoricSalariu :: Int -> [String] -> IO ()
afisareIstoricSalariu spatiuStanga [] = do
  setCursorColumn spatiuStanga
  putStrLn ""
  hFlush stdout
afisareIstoricSalariu spatiuStanga (cap : hist) = do
  setCursorColumn spatiuStanga
  putStrLn ("   -  " ++ cap)
  afisareIstoricSalariu spatiuStanga hist

afisareExperienta :: Int -> [Experienta] -> IO ()
afisareExperienta spatiuStanga [] = do
  setCursorColumn spatiuStanga
  hFlush stdout
afisareExperienta spatiuStanga (cap : experienta) = do
  setCursorColumn spatiuStanga
  putStrLn ("-  " ++ perioadaExperienta cap ++ ": " ++ companie cap ++ " (" ++ functie cap ++ ") ")
  setCursorColumn spatiuStanga
  putStrLn ("Istoric salariu (" ++ show (length (istoricSalariu cap)) ++ ")")
  afisareIstoricSalariu spatiuStanga (istoricSalariu cap)
  afisareExperienta spatiuStanga experienta

afisareCompletaAngajat :: Int -> Angajat -> IO ()
afisareCompletaAngajat spatiuStanga angajat = do
  let spatiuCol1 = spatiuStanga
  let spatiuCol2 = spatiuCol1 + 15
  setCursorPosition 1 spatiuStanga
  putStrLn "Informații angajat: "
  setCursorPosition 3 spatiuStanga
  putStrLn "1) Date personale"
  setCursorPosition 5 spatiuStanga
  let numeComplet = ((nume $ datePersonale angajat) ++ " " ++ (initiala $ datePersonale angajat) ++ "." ++ " " ++ (prenume $ datePersonale angajat))
  afisareRandInfoAngajat spatiuCol1 spatiuCol2 "MATRICOL"      (show $ matricol $ datePersonale angajat)
  afisareRandInfoAngajat spatiuCol1 spatiuCol2 "NUME"          numeComplet
  afisareRandInfoAngajat spatiuCol1 spatiuCol2 "DATA NAȘTERII" (dataNasterii $ datePersonale angajat)
  afisareRandInfoAngajat spatiuCol1 spatiuCol2 "ADRESA"        (adresa $ datePersonale angajat)
  afisareRandInfoAngajat spatiuCol1 spatiuCol2 "TELEFON"       (telefon $ datePersonale angajat)
  afisareRandInfoAngajat spatiuCol1 spatiuCol2 "E-MAIL"        (email $ datePersonale angajat)
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "2) Studii"
  putStrLn ""
  afisareStudii spatiuStanga (studii angajat)
  putStrLn "3) Experiență profesională"
  putStrLn ""
  afisareExperienta spatiuStanga (experienta angajat)
  _ <- getChar
  clearScreen
