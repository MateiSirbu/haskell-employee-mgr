import           Control.Exception
import           Data.List
import           System.Console.ANSI
import           System.Directory               ( createDirectoryIfMissing )
import           System.FilePath.Posix          ( takeDirectory )
import           System.IO
import           System.IO.Error

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
  { matricolFKStudii   :: Int
  , tipStudii          :: String
  , institutie         :: String
  , specializare       :: String
  , perioadaStudii     :: String
  , documenteAbsolvire :: [String]
  }

data Experienta = Experienta
  { matricolFKExperienta :: Int
  , companie             :: String
  , functie              :: String
  , perioadaExperienta   :: String
  , istoricSalariu       :: [Float]
  }

data Angajat = Angajat
  { datePersonale :: DatePersonale
  , studii        :: [Studii]
  , experienta    :: [Experienta]
  }

handlerCitire :: IOError -> IO [String]
handlerCitire e
  | isDoesNotExistError e = case ioeGetFileName (e) of
    Just cale -> (createDirectoryIfMissing True $ takeDirectory cale) >> writeFile cale "" >> return [""]
    Nothing   -> putStrLn (ioeGetErrorString (e)) >> return [""]
  | isPermissionError e = case ioeGetFileName (e) of
    Just cale -> putStrLn ("Acces interzis. Verificați permisiunile fișierului " ++ cale ++ ".") >> return [""]
    Nothing   -> putStrLn (ioeGetErrorString (e)) >> return [""]
  | otherwise = ioError e

citireFisier :: String -> IO [String]
citireFisier caleFisier = handle handlerCitire $ do
  continut <- readFile caleFisier
  let randuri = lines continut
  return randuri

citireAngajati :: () -> IO ()
citireAngajati () = do
  fisDatePersonale <- citireFisier "./date/DatePersonale.csv"
  fisStudii        <- citireFisier "./date/Studii.csv"
  fisExperienta    <- citireFisier "./date/Experienta.csv"
  print fisDatePersonale
  print fisStudii
  print fisExperienta

meniuPrincipal :: String -> IO ()
meniuPrincipal titlu = do
  setTitle titlu
  setCursorPosition 1 20
  putStrLn titlu
  hFlush stdout

main :: IO ()
main = do
  let titlu = "Evidența angajaților firmei Generic SRL"
  clearScreen
  meniuPrincipal titlu
  citireAngajati ()

