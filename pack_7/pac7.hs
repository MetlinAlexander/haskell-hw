-- Pack 7. IO
import System.IO
import qualified Data.ByteString as B
import System.Directory
import qualified Data.Text as Rep
-- task 1 Sanity Check
printFile :: String -> IO ()
printFile fileToPrint = do  
        handle <- openFile fileToPrint ReadMode
        contents <- hGetContents handle
        putStr contents
        hClose handle

--printFile "pac7.hs"


-- task 2 Text Check
areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText file_1 file_2 = do
        handle_1 <- openFile file_1 ReadMode
        handle_2 <- openFile file_2 ReadMode
        contents_1 <- hGetContents handle_1
        contents_2 <- hGetContents handle_2
        return (contents_1 == contents_2)

-- areEqualText "test1.txt" "test2.txt"
-- areEqualText "test1.txt" "pac7.hs"

-- task 3 Dos2Unix
dos2unix :: String -> IO ()
dos2unix fileToChange= do
        handle <- openFile fileToChange ReadMode
        contents <- hGetContents handle
        writeFile ("unix_temp.txt") (Rep.unpack ( Rep.replace (Rep.pack "\r\n") (Rep.pack "\n") (Rep.pack contents) ))
        hClose handle
        contents_with_changes <- readFile ("unix_temp.txt")
        writeFile fileToChange contents_with_changes
-- dos2unix "dox.txt"
unix2dos :: String -> IO ()
unix2dos fileToChange= do
        handle <- openFile fileToChange ReadMode
        contents <- hGetContents handle
        writeFile ("dox_temp.txt") (Rep.unpack ( Rep.replace (Rep.pack "\n") (Rep.pack "\r\n") (Rep.pack contents) ))
        hClose handle
        contents_with_changes <- readFile ("dox_temp.txt")
        writeFile fileToChange contents_with_changes

-- unix2dos "unix.txt"
-- task 4 Binary check

areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin file_1 file_2 = do
        handle_1 <- openFile file_1 ReadMode
        handle_2 <- openFile file_2 ReadMode
        contents_1 <- B.hGetContents handle_1
        contents_2 <- B.hGetContents handle_2
        return (contents_1 == contents_2)

-- areEqualBin "test1.txt" "test2.txt"
-- areEqualBin "test1.txt" "pac7.hs"

-- task 5 No Vimmers?
fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited filePath = do
        doesFileExist (filePath ++ ".sw") 

-- fileIsBeingEdited "README.md"
-- fileIsBeingEdited "pac7.hs"