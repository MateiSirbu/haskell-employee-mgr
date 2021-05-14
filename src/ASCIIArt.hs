module ASCIIArt where

import           GenericTable
import           System.Console.ANSI
import           System.IO

printSplashScreen :: () -> IO ()
printSplashScreen () = do
  putStrLn "             _____         _      _               _                          "
  putStrLn "            | ____|__   __(_)  __| |  ___  _ __  | |_  __ _                  "
  putStrLn "            |  _|  \\ \\ / /| | / _` | / _ \\| '_ \\ | __|/ _` |             "
  putStrLn "            | |___  \\ V / | || (_| ||  __/| | | || |_| (_| |                "
  putStrLn "            |_____|  \\_/  |_| \\__,_| \\___||_| |_| \\__|\\__,_|            "
  putStrLn "         _                           _         _    _  _                     "
  putStrLn "        / \\    _ __    __ _   __ _  (_)  __ _ | |_ (_)| |  ___   _ __       "
  putStrLn "       / _ \\  | '_ \\  / _` | / _` | | | / _` || __|| || | / _ \\ | '__|    "
  putStrLn "      / ___ \\ | | | || (_| || (_| | | || (_| || |_ | || || (_) || |         "
  putStrLn "     /_/   \\_\\|_| |_| \\__, | \\__,_|_/ | \\__,_| \\__||_||_| \\___/ |_|   "
  putStrLn "                      |___/       |__/                                       "

printGoodbyeScreen :: () -> IO ()
printGoodbyeScreen () = do
  clearScreen
  setCursorPosition 2 0
  putStrLn "            %%%%%%%% %#######                                  "
  putStrLn "              %%%%%%%& %#######                                "
  putStrLn "                %%%%%%%  #######&                              "
  putStrLn "                 %%%%%%%% &#######                             "
  putStrLn "                   %%%%%%%  #######% ###############           "
  putStrLn "                    &%%%%%%%  #######% #############           "
  putStrLn "                     %%%%%%%   ########                        "
  putStrLn "                    %%%%%%%  ###########  &#########           "
  putStrLn "                  %%%%%%%  %##############  (#######           "
  putStrLn "                %%%%%%%% &#######   ########                   "
  putStrLn "               %%%%%%%  #######       #######                  "
  putStrLn "             %%%%%%%  %#######         %#######                "
  putStrLn "                                                               "
  putStrLn "    _    ____    ____ ____ _  _ ____ ___  ____ ____ ____   /   "
  putStrLn "    |    |__|    |__/ |___ |  | |___ |  \\ |___ |__/ |___  /   "
  putStrLn "    |___ |  |    |  \\ |___  \\/  |___ |__/ |___ |  \\ |___ .  "
  putStrLn "                                                               "
  putStr "                        [Apăsați ENTER] "
  hFlush stdout
  _ <- getChar
  clearScreen
  setCursorPosition 0 0

printWarning :: Int -> Int -> Int -> IO ()
printWarning linie spatiuStanga lungimeEcran = do
  printMessage linie spatiuStanga "╔╗" lungimeEcran
  printMessage (linie + 1) spatiuStanga "║║" lungimeEcran
  printMessage (linie + 2) spatiuStanga "║║" lungimeEcran
  printMessage (linie + 3) spatiuStanga "╚╝" lungimeEcran
  printMessage (linie + 4) spatiuStanga "╔╗" lungimeEcran
  printMessage (linie + 5) spatiuStanga "╚╝" lungimeEcran