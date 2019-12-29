import Data.List
import System.IO
import Distribution.Simple.Setup
import Data.Map (fromListWith, toList)
import Data.Maybe
import System.Directory --does file exist?
import Data.Char(toLower,toUpper)
import Data.Char ( isLetter, isSpace, isAlpha )
import Data.String
import Control.Monad

-- Contorizare aparitii cuvant
--Lista ordonata
getSortedFrequency::[Char]->[[Char]]->Int
getSortedFrequency _[]=0
getSortedFrequency s (c:rl)
  | (s==c)=1+getSortedFrequency s rl
  | otherwise=0

--Eliminare aparitii cuvant specificat
--Lista ordonata
removeSortedOccurences::String->[String]->[String]
removeSortedOccurences _[]=[]
removeSortedOccurences s (c:rl)
 |(s==c)=removeSortedOccurences s rl
 | otherwise=c:rl

-- Frecventa tuturor cuvintelor
getAllWordsFrequency::[String]->[(String,Int)]
getAllWordsFrequency []=[]
getAllWordsFrequency (c:rl)=(c,1+(getSortedFrequency c rl)):getAllWordsFrequency (removeSortedOccurences c rl)

-- Frecventa unui singur cuvant
countOf::String->String->Int
countOf s x = do { length $ filter (\w -> w == s) $ words x}

--Continutul fisierului de analizat--
readFromFile :: String -> IO String
readFromFile fileName = do
  theFile <- openFile fileName ReadMode;
  contents <- hGetContents theFile;
  return contents;

--Preluarea fisierului care urmeaza sa fie analizat--
getFileToParse::IO String
getFileToParse = do {
    putStrLn "Introduceti numele fisierului";
    fileName <- getLine;
    return fileName;
}

--Verificarea existentei fisierului--
getExistingFile::String->IO String
getExistingFile fileName = do {
    condition <- doesFileExist fileName;
    if not condition 
        then do { putStrLn "Fisierul nu exista.";
                  fileName <- getFileToParse;
                  getExistingFile fileName;}
        else return fileName;
}

removeNonLetters :: [Char] -> [String]
removeNonLetters = words . filter (liftM2 (||) isAlpha isSpace)

-- Afisare statistica
-- Varianta ecran
printResult::[(String,Int)]->IO()
printResult []=putStrLn "**************************"
printResult ((cuv,frec):rl)=do
      putColoana cuv 15
      putColoana (show frec) 9
      putStrLn "|"
      printResult rl

-- Afisare coloana
-- Varianta ecran
putColoana::String->Int->IO()
putColoana cuv ncar=do
                   putStr cuv
                   hFlush stdout
                   space (ncar-(length cuv))

-- Adaugare spatii in scop de aliniere
-- Varianta ecran
space::Int->IO()
space 0=putStr ""
space 1=putStr " "
space n=putStr " ">>space (n-1)

-- Generare raport in fisier
-- de handle specificat
fGenRap::Handle->[(String,Int)]->IO()
fGenRap h []=do
              hPutStrLn h "*************************"
              hClose h
fGenRap h ((cuv,frec):rl)=do
           hputColoana h cuv 15
           hputColoana h (show frec) 9
           hPutStrLn h "|"
           fGenRap h rl

-- Afisare coloana
-- Varianta fisier           
hputColoana::Handle->String->Int->IO()
hputColoana h cuv ncar=do
                   hPutStr h cuv
                   hFlush h
                   hSpace h (ncar-(length cuv))            

-- Adaugare spatii in scop de aliniere
-- Varianta fisier
hSpace::Handle->Int->IO()
hSpace h 0=hPutStr h ""
hSpace h 1=hPutStr h " "
hSpace h n=hPutStr h " ">>hSpace h (n-1)

--Main function--
main::IO()
main=do
  
  --Citim de la tastatura numele fisierului--
  file_name <- getFileToParse;
  
  checked_file <- getExistingFile file_name;
  
  --Stocam continutul fisierului intr-o variabila
  contentFile <- readFromFile checked_file;
  
  --Convertim continutul fisierului to lowercase
  --String, text intreg --
  let lowContents = map toLower contentFile;

  --Lista de cuvinte--
  let lowContentWords = words lowContents;
  let parsedWords = removeNonLetters lowContents;
  sortedWords <- return (sort parsedWords);
 
 -- Determinarea statisticii       
  rez<-return (getAllWordsFrequency sortedWords)
       
 -- Afisare statistica pe ecran
  
 -- printResult rez
  
  putStrLn "Optiunile programului:";
  putStrLn "*******************";
  putStrLn "1-Frecventa tuturor cuvintelor.";
  putStrLn "2-Frecventa unui cuvant precizat.";
  putStrLn "3-Terminare program";
  putStrLn "*******************";
  putStrLn "Ce optiune doriti:";
  opt<-getLine;
    if (opt=="1") 
     then do { 

--In fisier--
      h<-openFile "Raport.txt" WriteMode;
      hPutStrLn h "**************************";
      hPutStrLn h "Cuvant         Frecventa ";
      hPutStrLn h "**************************";
      fGenRap h rez;

--Afisare--
      putStrLn "";
      putStrLn "";
      putStrLn "**************************";
      putStrLn "Cuvant         Frecventa ";
      putStrLn "**************************";
      printResult rez}
      else if (opt=="2")
      then do {
        givenWord<-getLine;
        freq<-return (countOf givenWord lowContents);
        putStrLn("Frecventa cuvantului dat:" ++givenWord);
        print freq} 
           else putStrLn "La revedere....";