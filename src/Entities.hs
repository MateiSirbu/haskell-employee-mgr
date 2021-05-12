-- =======================================
-- Definiție entități
--
-- Sîrbu Matei-Dan, grupa 10LF383, UniTBv
-- http://github.msirbu.eu
-- =======================================

module Entities where

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
