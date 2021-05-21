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

scriereItemiExperienta :: Int -> Int -> Int -> Int -> [Experienta] -> IO [Experienta]
scriereItemiExperienta _ _ 0 matricol ioExperienta = do
  return ioExperienta
scriereItemiExperienta spatiuStanga lungimeEcran nrItemi matricol ioExperienta = do
  hFlush stdout
  let experienta     = ioExperienta
  let itemExperienta = Experienta{}
  putStrLn "Cartof"
  scriereItemiExperienta spatiuStanga lungimeEcran (nrItemi - 1) matricol (experienta ++ [itemExperienta])

scriereItemiStudii :: Int -> Int -> Int -> Int -> [Studii] -> IO [Studii]
scriereItemiStudii _ _ 0 matricol ioStudii = do
  return ioStudii
scriereItemiStudii spatiuStanga lungimeEcran nrItemi matricol ioStudii = do
  hFlush stdout
  let studii     = ioStudii
  let itemStudiu = Studii{}
  putStrLn "Ceapă"
  scriereItemiStudii spatiuStanga lungimeEcran (nrItemi - 1) matricol (ioStudii ++ [itemStudiu])

scriereStudii :: Int -> Int -> Int -> IO [Studii]
scriereStudii spatiuStanga lungimeEcran matricol = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "2) Studii"
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Câți itemi? > "
  hFlush stdout
  nrItemi <- getLine
  putStrLn ""
  studii <- scriereItemiStudii spatiuStanga lungimeEcran (read nrItemi) matricol []
  return []

scriereExperienta :: Int -> Int -> Int -> IO [Experienta]
scriereExperienta spatiuStanga lungimeEcran matricol = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "3) Experiență"
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Câți itemi? > "
  hFlush stdout
  nrItemi <- getLine
  putStrLn ""
  studii <- scriereItemiExperienta spatiuStanga lungimeEcran (read nrItemi) matricol []
  return []

citireAngajatFormular :: Int -> Int -> [Angajat] -> IO Angajat
citireAngajatFormular spatiuStanga lungimeEcran angajatiExistenti = do
  datePersonale <- scriereDatePersonale spatiuStanga lungimeEcran angajatiExistenti
  studii        <- scriereStudii spatiuStanga lungimeEcran $ matricol datePersonale
  experienta    <- scriereExperienta spatiuStanga lungimeEcran $ matricol datePersonale
  return Angajat { datePersonale = datePersonale, studii = studii, experienta = experienta }
