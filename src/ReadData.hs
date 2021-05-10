module ReadData where

import           Control.Exception
import           System.IO
import           System.IO.Error
import           System.Directory               ( createDirectoryIfMissing )
import           System.FilePath.Posix          ( takeDirectory )

data DatePersonale = DatePersonale
  { matricol     :: Int
  , nume         :: String
  , initiala     :: String
  , prenume      :: String
  , dataNasterii :: String
  , adresa       :: String
  , telefon      :: String
  , email        :: String
  }

data Studii = Studii
  { matricolStudiiFK   :: Int
  , tipStudii          :: String
  , institutie         :: String
  , specializare       :: String
  , perioadaStudii     :: String
  , documenteAbsolvire :: [String]
  }

data Experienta = Experienta
  { matricolExperientaFK :: Int
  , companie             :: String
  , functie              :: String
  , perioadaExperienta   :: String
  , istoricSalariu       :: [String]
  }

data Angajat = Angajat
  { datePersonale :: DatePersonale
  , studii        :: [Studii]
  , experienta    :: [Experienta]
  }

conversieRandLaDatePersonale :: [String] -> DatePersonale
conversieRandLaDatePersonale rand = DatePersonale { matricol     = read (rand !! 0)
                                                  , nume         = (rand !! 1)
                                                  , initiala     = (rand !! 2)
                                                  , prenume      = (rand !! 3)
                                                  , dataNasterii = (rand !! 4)
                                                  , adresa       = (rand !! 5)
                                                  , telefon      = (rand !! 6)
                                                  , email        = (rand !! 7)
                                                  }

conversieRandLaStudii :: [String] -> Studii
conversieRandLaStudii rand = Studii { matricolStudiiFK   = read (rand !! 0)
                                    , tipStudii          = (rand !! 1)
                                    , institutie         = (rand !! 2)
                                    , specializare       = (rand !! 3)
                                    , perioadaStudii     = (rand !! 4)
                                    , documenteAbsolvire = tokenizare (rand !! 5) ';'
                                    }

conversieRandLaExperienta :: [String] -> Experienta
conversieRandLaExperienta rand = Experienta { matricolExperientaFK = read (rand !! 0)
                                            , companie             = (rand !! 1)
                                            , functie              = (rand !! 2)
                                            , perioadaExperienta   = (rand !! 3)
                                            , istoricSalariu       = tokenizare (rand !! 4) ';'
                                            }

tokenizare :: String -> Char -> [String]
tokenizare [] _ = [""]
tokenizare (chr : chrs) sep | chr == sep = "" : restul
                            | otherwise  = (chr : head restul) : tail restul
  where restul = tokenizare chrs sep

citireDatePersonale :: [String] -> [DatePersonale]
citireDatePersonale []     = []
citireDatePersonale fisier = map (\rand -> (conversieRandLaDatePersonale (tokenizare rand ','))) fisier

citireStudii :: [String] -> [Studii]
citireStudii []     = []
citireStudii fisier = map (\rand -> (conversieRandLaStudii (tokenizare rand ','))) fisier

citireExperienta :: [String] -> [Experienta]
citireExperienta []     = []
citireExperienta fisier = map (\rand -> (conversieRandLaExperienta (tokenizare rand ','))) fisier

citireFisier :: String -> IO [String]
citireFisier caleFisier = handle handlerCitire $ do
  continut <- readFile caleFisier
  let randuri = lines continut
  return randuri

handlerCitire :: IOError -> IO [String]
handlerCitire e
  | isDoesNotExistError e = case ioeGetFileName (e) of
    Just cale -> (createDirectoryIfMissing True $ takeDirectory cale) >> writeFile cale "" >> return []
    Nothing   -> putStrLn (ioeGetErrorString (e)) >> return []
  | isPermissionError e = case ioeGetFileName (e) of
    Just cale -> putStrLn ("Acces interzis. Verificați permisiunile fișierului " ++ cale ++ ".") >> return []
    Nothing   -> putStrLn (ioeGetErrorString (e)) >> return []
  | otherwise = putStrLn ("Nu se poate citi fișierul.") >> return []

initializareAngajat :: DatePersonale -> IO Angajat
initializareAngajat date = do
  fisStudii <- citireFisier "./date/Studii.csv"
  let lstStudii = filter (\studiu -> ((matricolStudiiFK studiu) == matricol date)) (citireStudii fisStudii)
  fisExperienta <- citireFisier "./date/Experienta.csv"
  let lstExperienta = filter (\exp -> ((matricolExperientaFK exp) == matricol date)) (citireExperienta fisExperienta)
  let angajat       = Angajat { datePersonale = date, studii = lstStudii, experienta = lstExperienta }
  return angajat

citireAngajati :: () -> IO [IO Angajat]
citireAngajati () = do
  fisDatePersonale <- citireFisier "./date/DatePersonale.csv"
  let lstDatePersonale = citireDatePersonale fisDatePersonale
  let angajati         = map initializareAngajat lstDatePersonale
  return angajati
