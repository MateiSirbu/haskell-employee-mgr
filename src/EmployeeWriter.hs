module EmployeeWriter where

import           ASCIIArt
import           DataReader
import           Entities
import           GenericTable
import           System.Console.ANSI
import           System.IO

citireCamp :: Int -> Int -> String -> IO String
citireCamp spCol1 spCol2 numeCamp = do
  setCursorColumn spCol1
  putStr numeCamp
  hFlush stdout
  setCursorColumn spCol2
  putStr "> "
  hFlush stdout
  camp <- getLine
  return camp

eroareDuplicat :: Int -> Int -> IO ()
eroareDuplicat spatiuStanga lungimeEcran = do
  clearScreen
  printWarning 2 spatiuStanga lungimeEcran
  printMessage 9  spatiuStanga "EROARE"                         lungimeEcran
  printMessage 11 spatiuStanga "Există deja un angajat cu numărul matricol" lungimeEcran
  printMessage 12 spatiuStanga "specificat. Încercați din nou." lungimeEcran
  printMessage 14 spatiuStanga "[Apăsați ENTER] "               lungimeEcran
  _ <- getChar
  clearScreen

scriereDatePersonale :: Int -> Int -> [Angajat] -> IO DatePersonale
scriereDatePersonale spCol1 lungimeEcran angajatiExistenti = do
  let spCol2 = spCol1 + 20
  setCursorPosition 5 0
  setCursorPosition 1 spCol1
  printMessage 1 spCol1 "Adăugare angajat" lungimeEcran
  setCursorPosition 3 spCol1
  putStrLn "1) Date personale"
  putStrLn ""
  matricol <- citireCamp spCol1 spCol2 "MATRICOL"
  let cautare = cautareAngajat matricol angajatiExistenti
  if length cautare > 0
    then do
      eroareDuplicat spCol1 lungimeEcran
      scriereDatePersonale spCol1 lungimeEcran angajatiExistenti
    else do
      nume         <- citireCamp spCol1 spCol2 "NUME DE FAMILIE"
      initiala     <- citireCamp spCol1 spCol2 "INIȚIALĂ"
      prenume      <- citireCamp spCol1 spCol2 "PRENUME"
      dataNasterii <- citireCamp spCol1 spCol2 "DATA NAȘTERII"
      adresa       <- citireCamp spCol1 spCol2 "ADRESA"
      telefon      <- citireCamp spCol1 spCol2 "TELEFON"
      email        <- citireCamp spCol1 spCol2 "E-MAIL"
      let dp = DatePersonale { matricol     = read matricol
                             , nume         = nume
                             , initiala     = initiala
                             , prenume      = prenume
                             , dataNasterii = dataNasterii
                             , adresa       = adresa
                             , telefon      = telefon
                             , email        = email
                             }
      return dp

scriereStudii :: Int -> Int -> IO [Studii]
scriereStudii spatiuStanga lungimeEcran = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "2) Studii"
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Câți itemi? > "
  hFlush stdout
  _ <- getLine
  return []

scriereExperienta :: Int -> Int -> IO [Experienta]
scriereExperienta spatiuStanga lungimeEcran = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "3) Experienta"
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Câți itemi? > "
  hFlush stdout
  _ <- getLine
  return []

citireAngajatFormular :: Int -> Int -> [Angajat] -> IO Angajat
citireAngajatFormular spatiuStanga lungimeEcran angajatiExistenti = do
  datePersonale <- scriereDatePersonale spatiuStanga lungimeEcran angajatiExistenti
  studii        <- scriereStudii spatiuStanga lungimeEcran
  experienta    <- scriereExperienta spatiuStanga lungimeEcran
  return Angajat { datePersonale = datePersonale, studii = studii, experienta = experienta }
