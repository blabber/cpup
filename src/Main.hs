-- vim: et:ts=2:sw=2
module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Error

respectCase :: Bool
respectCase = True

data Flag = Verbose | Force
  deriving (Eq)

usage :: String
usage = usageInfo header options
  where header = "usage: cpup [OPTIONS] <srcdir> <tgtdir>"

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "verbose output"
          , Option ['f'] ["force"]   (NoArg Force)   "copy regardless of modification time"
          ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> if length n /= 2
                    then die usage
                    else return (o, n)
    (_, _, e)  -> die usage

main :: IO ()
main = do
  (flags, (src : tgt : _)) <- getArgs >>= parseOptions
  srcFiles <- listFiles src
  tgtFiles <- listFiles tgt
  srcNewerFiles <- filterM (isSrcNewer flags src tgt) $ filterCandidates srcFiles tgtFiles
  mapM_ (copyFile' flags src tgt) srcNewerFiles
  putStrLn $ src ++ " -> " ++ tgt ++ ": " ++ (show $ length srcNewerFiles) ++ " file(s)"

listFiles :: FilePath -> IO [FilePath]
listFiles d = do
  l <- listDirectory d
  filterM doesFileExist' l
  where
    doesFileExist' l = doesFileExist $ d </> l

filterCandidates :: [FilePath] -> [FilePath] -> [FilePath]
filterCandidates a b
  | respectCase = intersect a b
  | otherwise   = intersectBy (\x y -> y == map toUpper x) a $ map (map toUpper) b

isSrcNewer :: [Flag] -> FilePath -> FilePath -> FilePath -> IO Bool
isSrcNewer flags s t f
  | Force `elem` flags = return (True)
isSrcNewer flags s t f = do
  srcFileTime <- getModificationTime $ s </> f
  tgtFileTime <- getModificationTime $ t </> f
  return $ srcFileTime > tgtFileTime

copyFile' :: [Flag] -> FilePath -> FilePath -> FilePath -> IO ()
copyFile' flags s t f = do
  let srcFile = s </> f
      tgtFile = t </> f
  when (Verbose `elem` flags) $ putStrLn $ srcFile ++ " -> " ++ tgtFile
  catch (copyFile srcFile tgtFile)
        (\ e -> do let err = e :: IOException
                       d   = ioeGetErrorString err
                   putStrLn $ "Could not copy file \"" ++ f ++ "\": " ++ d)
