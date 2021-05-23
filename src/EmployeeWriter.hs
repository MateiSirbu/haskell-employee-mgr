-- =======================================
-- Scriere listă angajați în fișiere CSV
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module EmployeeWriter where

import           ASCIIArt
import           Data.List
import           DataReader
import           Entities
import           GenericTable
import           System.Console.ANSI
import           System.IO
import           Text.Read

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

citireNumarItemi :: String -> Int
citireNumarItemi x | ((readMaybe x :: Maybe Int) == Nothing) = 0
                   | (read x < 0) = 0
                   | otherwise = read x :: Int

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

citireDatePersonaleTastatura :: Int -> Int -> [Angajat] -> IO DatePersonale
citireDatePersonaleTastatura spCol1 lungimeEcran angajatiExistenti = do
  let spCol2 = spCol1 + 35
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
      citireDatePersonaleTastatura spCol1 lungimeEcran angajatiExistenti
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

citireItemString :: Int -> Int -> [String] -> IO [String]
citireItemString _ 0 strings = do
  return strings
citireItemString spatiuStanga nrItemi strings = do
  setCursorColumn spatiuStanga
  putStr "> "
  hFlush stdout
  str <- getLine
  citireItemString spatiuStanga (nrItemi - 1) (strings ++ [str])

scriereItemiExperienta :: Int -> Int -> Int -> [Experienta] -> IO [Experienta]
scriereItemiExperienta _ 0 matricol ioExperienta = do
  return ioExperienta
scriereItemiExperienta spatiuStanga nrItemi matricol ioExperienta = do
  let spCol1     = spatiuStanga
  let spCol2     = spCol1 + 35
  let experienta = ioExperienta
  putStrLn ""
  companie              <- citireCamp spCol1 spCol2 "COMPANIE"
  functie               <- citireCamp spCol1 spCol2 "FUNCȚIE"
  perioadaExperienta    <- citireCamp spCol1 spCol2 "PERIOADĂ EXPERIENȚĂ"
  nrItemiIstoricSalariu <- citireCamp spCol1 spCol2 "ISTORIC SALARIU: Câți itemi?"
  istoricSalariu        <- citireItemString spCol2 (citireNumarItemi nrItemiIstoricSalariu) []
  let itemExperienta = Experienta { matricolExperientaFK = matricol
                                  , companie             = companie
                                  , functie              = functie
                                  , perioadaExperienta   = perioadaExperienta
                                  , istoricSalariu       = istoricSalariu
                                  }
  scriereItemiExperienta spatiuStanga (nrItemi - 1) matricol (experienta ++ [itemExperienta])

citireItemiStudii :: Int -> Int -> Int -> [Studii] -> IO [Studii]
citireItemiStudii _ 0 matricol ioStudii = do
  return ioStudii
citireItemiStudii spatiuStanga nrItemi matricol ioStudii = do
  let spCol1 = spatiuStanga
  let spCol2 = spCol1 + 35
  let studii = ioStudii
  putStrLn ""
  tipStudii                 <- citireCamp spCol1 spCol2 "TIP STUDII"
  institutie                <- citireCamp spCol1 spCol2 "INSTITUȚIE"
  specializare              <- citireCamp spCol1 spCol2 "SPECIALIZARE"
  perioadaStudii            <- citireCamp spCol1 spCol2 "PERIOADĂ STUDII"
  nrItemiDocumenteAbsolvire <- citireCamp spCol1 spCol2 "DOCUMENTE ABSOLVIRE: Câți itemi?"

  documenteAbsolvire        <- citireItemString spCol2 (citireNumarItemi nrItemiDocumenteAbsolvire) []
  let itemStudiu = Studii { matricolStudiiFK   = matricol
                          , tipStudii          = tipStudii
                          , institutie         = institutie
                          , specializare       = specializare
                          , perioadaStudii     = perioadaStudii
                          , documenteAbsolvire = documenteAbsolvire
                          }
  citireItemiStudii spatiuStanga (nrItemi - 1) matricol (ioStudii ++ [itemStudiu])

citireStudiiTastatura :: Int -> Int -> IO [Studii]
citireStudiiTastatura spatiuStanga matricol = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "2) Studii"
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Câți itemi? > "
  hFlush stdout
  nrItemi <- getLine
  studii  <- citireItemiStudii spatiuStanga (citireNumarItemi nrItemi) matricol []
  return studii

citireExperientaTastatura :: Int -> Int -> IO [Experienta]
citireExperientaTastatura spatiuStanga matricol = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStrLn "3) Experiență"
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Câți itemi? > "
  hFlush stdout
  nrItemi    <- getLine
  experienta <- scriereItemiExperienta spatiuStanga (citireNumarItemi nrItemi) matricol []
  return experienta

citireAngajatFormular :: Int -> Int -> [Angajat] -> IO Angajat
citireAngajatFormular spatiuStanga lungimeEcran angajatiExistenti = do
  datePersonale <- citireDatePersonaleTastatura spatiuStanga lungimeEcran angajatiExistenti
  studii        <- citireStudiiTastatura spatiuStanga $ matricol datePersonale
  experienta    <- citireExperientaTastatura spatiuStanga $ matricol datePersonale
  return Angajat { datePersonale = datePersonale, studii = studii, experienta = experienta }

csvStudii :: Studii -> String
csvStudii s = do
  let documenteAbsolvireDelim = concat (intersperse "|" $ documenteAbsolvire s)
  ((show $ matricolStudiiFK s) ++ "," ++ tipStudii s ++ "," ++ institutie s ++ "," ++ specializare s ++ "," ++ perioadaStudii s ++ "," ++ documenteAbsolvireDelim)

csvExperienta :: Experienta -> String
csvExperienta e = do
  let delimIstoricSalariu = concat (intersperse "|" $ istoricSalariu e)
  ((show $ matricolExperientaFK e) ++ "," ++ companie e ++ "," ++ functie e ++ "," ++ perioadaExperienta e ++ "," ++ delimIstoricSalariu)

csvDatePersonale :: DatePersonale -> String
csvDatePersonale dp =
  (show $ matricol dp)
    ++ ","
    ++ nume dp
    ++ ","
    ++ initiala dp
    ++ ","
    ++ prenume dp
    ++ ","
    ++ dataNasterii dp
    ++ ","
    ++ adresa dp
    ++ ","
    ++ telefon dp
    ++ ","
    ++ email dp

adaugareAngajatInFisier :: Int -> Angajat -> IO ()
adaugareAngajatInFisier spatiuStanga angajat = do
  putStrLn ""
  setCursorColumn spatiuStanga
  putStr "Confirmați adăugarea angajatului? (D/N) > "
  hFlush stdout
  optiune <- getLine
  if optiune == "d" || optiune == "D"
    then do
      hDP <- openFile "./date/DatePersonale.csv" AppendMode
      hPutStrLn hDP $ csvDatePersonale $ datePersonale angajat
      hClose hDP
      hS <- openFile "./date/Studii.csv" AppendMode
      mapM
        (\s -> do
          hPutStrLn hS (csvStudii s)
        )
        (studii angajat)
      hClose hS
      hE <- openFile "./date/Experienta.csv" AppendMode
      mapM
        (\e -> do
          hPutStrLn hE (csvExperienta e)
        )
        (experienta angajat)
      hClose hE
      clearScreen
    else clearScreen
