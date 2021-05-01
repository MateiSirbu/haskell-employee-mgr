import           Control.Exception
import           Data.List
import           System.Console.ANSI
import           System.IO

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

citireFisier :: String -> IO [String]
citireFisier numeFisier = do
  continutTry <- try (readFile numeFisier) :: IO (Either SomeException String)
  continut    <- case continutTry of
    Left  _     -> putStrLn "Funcția a făcut buba" >> return ""
    Right value -> return value
  let randuri = lines continut
  return randuri

citireAngajati :: () -> IO ()
citireAngajati () = do
  fisDatePersonale <- citireFisier "DatePersonale.csv"
  fisStudii        <- citireFisier "Studii.txt"
  fisExperienta    <- citireFisier "Experienta.txt"
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

