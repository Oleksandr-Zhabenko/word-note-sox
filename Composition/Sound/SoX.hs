-- |
-- Module      :  Composition.Sound.SoX
-- Copyright   :  (c) Oleksandr Zhabenko 2019-2024
-- License     :  MIT
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Some functionality that is primarily implemented using 'String' and 'System.Process' in the algorithmic-composition-basic and related packages. Is rewritten and groupped to be probably more suitable for concurrent and asynchronous usage.

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists #-}
{-# OPTIONS_HADDOCK -show-extensions #-}


module Composition.Sound.SoX where

import GHC.Base hiding (foldr) 
import GHC.Num ((+),(*))
import GHC.Real (fromIntegral,rem,(/))
import Text.Show (Show(..))
import Text.Read
import Data.Tuple (fst)
import Data.List hiding (lines,words,take,head,uncons,foldr,dropWhile) 
import System.Directory
import System.IO (FilePath, stderr, putStrLn)
import Data.ByteString.Lazy hiding (null,take,filter,isSuffixOf,foldr,dropWhile)
import Data.ByteString.Lazy.Char8 (lines,words,take,foldr,dropWhile)  
--import System.Exit (ExitCode(ExitSuccess))
import EndOfExe2 (showE0Dup)
import Numeric (showFFloat)
import Data.InsertLeft (takeFromEndG, splitAtEndG)
import System.Process.Typed

type SoXEffects = [String]

-- Taken from the Composition.Sound.IntermediateF module from @algorithmic-composition-basic@ here so that they are more used this way.  

-- | Takes a filename to be applied a SoX chain of effects as list of 'String' (the second argument). Produces the temporary
-- new file with the name ((name-of-the-file) ++ (\"effects.wav\"  OR \"effects.flac\") -- the type is preserved), which then is removed. 
--
-- The syntaxis is that every separate literal for SoX must be a new element in the list. Please, for more information, refer to SoX documentation.
-- Please, check by yourself whether you have enough permissions to work with the corresponding 'FilePath's.
soxE :: FilePath -> SoXEffects -> IO ()
soxE file arggs = do
  let effile = file ++ "effects" ++ efw2 file
  (code,_,herr) <- readProcess (proc (showE0Dup "sox") ([file,effile] ++ arggs))
  case code of
    ExitSuccess -> renameFile effile file
    _ -> do
       exist <- doesFileExist effile
       if exist then do 
                  hPut stderr herr
                  removeFile effile
                  putStrLn $ "Composition.Sound.SoX.soxE: Applying SoX on the file \"" ++ file ++ "\" has not been successful. The file " ++ file ++ " has not been changed at all. "
       else do
         hPut stderr herr
         putStrLn $ "Composition.Sound.SoX.soxE: Creation of the file \"" ++ effile ++ "\" has not been successful. The file " ++ file ++ " has not been changed at all. "

w2f :: FilePath -> FilePath
w2f file 
  | ts == ".wav" = zs ++ ".flac" 
  | otherwise = error "Composition.Sound.SoX.w2f: The file is not a WAV file! "
      where (zs,ts) = splitAtEndG 4 file  
{-# INLINE w2f #-}

f2w :: FilePath -> FilePath
f2w file 
  | ts == ".flac" = zs ++ ".wav" 
  | otherwise = error "Composition.Sound.SoX.f2w: The file is not a FLAC file! "
      where (zs,ts) = splitAtEndG 5 file
{-# INLINE f2w #-}

wOrf :: FilePath -> String
wOrf file 
  | us == ".wav" = "w"
  | us == "flac" = "f"
  | otherwise = error "Composition.Sound.SoX.wOrf: The file is neither a WAV nor a FLAC file!"
      where us = takeFromEndG 4 file 
{-# INLINE wOrf #-}

cfw2wf :: FilePath -> FilePath
cfw2wf file
 | wf == "w" = w2f file
 | wf == "f" = f2w file
 | otherwise = error "Composition.Sound.SoX.cfw2wf: The file is neither a WAV nor a FLAC file! "
     where wf = wOrf file
{-# INLINE cfw2wf #-}

efw2 :: FilePath -> String
efw2 file 
 | us == ".wav" = us
 | us == "flac" = '.':us
 | otherwise = error "Composition.Sound.SoX.efw2: The file is neither a WAV nor a FLAC file!"
     where us = takeFromEndG 4 file
{-# INLINE efw2 #-}

efw2vv :: FilePath -> String
efw2vv file 
 | us == ".wav" = ".flac"
 | us == "flac" = ".wav"
 | otherwise = error "Composition.Sound.SoX.efw2vv: The file is neither a WAV nor a FLAC file! "
     where us = takeFromEndG 4 file 
{-# INLINE efw2vv #-}

--------------------------

-- | Applies \"fade q\" effect to both ends of the supported by SoX sound file 'FilePath' so that concatenating them consequently after such application 
-- leads to no clipping. Otherwise, the clipping exists if not prevented by may be some other means. For more information, please, refer to the
-- SoX documentation.
fadeEnds :: FilePath -> IO ()
fadeEnds = fadeEndsMilN 10
{-# INLINE fadeEnds #-}

-- | Applies \"fade q\" effect to both ends of the supported by SoX sound file 'FilePath' so that concatenating them consequently after such application 
-- leads to no clipping. Otherwise, the clipping exists if not prevented by may be some other means. The duration of the changes are usually 
-- smaller than for 'fadeEnds' function and is equal to 0.001 \* n sec (where n is in range [1..10]). 
-- For more information, please, refer to the SoX documentation.
fadeEndsMilN :: Int -> FilePath -> IO ()
fadeEndsMilN n file = soxE file ["fade","q", showFFloat (Just 4) (if (n `rem` 11) /= 0 then 0.001 * fromIntegral (n `rem` 11) else 0.002) "","-0.0"]

-- | Applies \"fade\" effect (the type is specified by the 'Char' argument, for more information, please, refer to the SoX documentation) to the both ends 
-- of the sound with header (supported by SoX). The 'Float' arguments specify the percentages of the length of the sound that is faded-in and faded-out 
-- respectively. Otherwise, the function returns an error.
fadeEndsTMN :: Char -> Float -> Float -> FilePath -> IO ()
fadeEndsTMN c per1 per2 file 
 | compare per1 0 == GT && compare per2 0 == GT && compare (per1 + per2) 100 /= GT = do
    d0 <- durationA file
    soxE file ["fade", case c of {'h' -> "h"; 'p' -> "p"; 'q' -> "q"; 't' -> "t"; ~_ -> "l"}, showFFloat (Just 4) (d0 * per1 / 100.0) "","-0.0", 
      showFFloat (Just 4) (d0 * per2 / 100.0) ""]
 | otherwise = error "Composition.Sound.SoX.fadeEndsTMN: the percentages sum is out of the (0..100] range. "

-- | Variant of the 'fadeEndsTMN' with the both equal percentages specified by the 'Float' argument. It must be in the range (0..50]. Otherwise, the function 
-- returns error.
fadeEndsTMB :: Char -> Float -> FilePath -> IO ()
fadeEndsTMB c per 
 | per > 0 && per <= 50 = fadeEndsTMN c per per
 | otherwise = error "Composition.Sound.SoX.fadeEndsTMB: The percentage is out of the (0..50] range. "
{-# INLINE fadeEndsTMB #-}

----------------------------------
--

takeU :: ByteString -> ByteString
takeU u
 | take 1 u == "-" = take 9 u
 | otherwise = take 8 u
{-# INLINE takeU #-}

-- | Function 'getMaxAG' returns a maximum amplitude of the sound in the file in the given lower and upper bounds represented as a tuple of 'Int' values.
getMaxAG :: ULencode -> FilePath -> (Int, Int) -> IO ByteString
getMaxAG ul file (lowerbound, upperbound) 
  | null . showE0Dup $ "sox" =   error "Composition.Sound.SoX.getMinAG: The SoX executable is not properly installed."
  | otherwise = do
     (_, _, herr) <- soxOpG1 ul [] file [] ["trim", show lowerbound ++ "s", "=" ++ show upperbound ++ "s", "stat"]
     let zs = lines herr 
     return (let u = (words $ zs !! 3) !! 2 in takeU u)

-- | Function 'getMinAG' returns a minimum amplitude of the sound in the file in the given lower and upper bounds represented as a tuple of 'Int' values.
getMinAG :: ULencode -> FilePath -> (Int, Int) -> IO ByteString
getMinAG ul file (lowerbound, upperbound) 
  | null . showE0Dup $ "sox" = error "Composition.Sound.SoX.getMinAG: The SoX executable is not properly installed."
  | otherwise = do
      (_, _, herr1) <- soxOpG1 ul [] file [] ["trim", show lowerbound ++ "s", "=" ++ show upperbound ++ "s", "stat"]
      let zs = lines herr1
      return (let u = (words $ zs !! 4) !! 2 in takeU u)
 
-- | Function 'selMaxAbsG' returns a maximum by absolute value amplitude of the sound and allows by its second value in the tuple determine whether it is a maximum or minimum.
-- Bool 'True' corresponds to maximum value, 'False' - to minimum value.
selMaxAbsG :: ULencode -> FilePath -> (Int, Int) -> IO (ByteString, Bool)
selMaxAbsG ul file (lowerbnd, upperbnd) = do
  tX <- getMaxAG ul file (lowerbnd, upperbnd)
  tN <- getMinAG ul file (lowerbnd, upperbnd)
  return (maxAbs (tX, tN))

data ULencode = W | UL1 | UL0 | UL deriving (Eq, Ord)

instance Show ULencode where
  show W = "(False, False)" -- Only working with .wav files.
  show UL1 = "(False, True)" -- .ul appears.
  show UL0 = "(True, False)" -- .ul disappears.
  show _ = "(True, True)" -- .ul is constantly used.

class SoundFileExts a where
  getExts :: a -> (String,String)
  isFileExtsR :: a -> FilePath -> FilePath -> Bool
  isFileExtsR ul file1 file2 = xs `isSuffixOf` file1 && ys `isSuffixOf` file2
    where (xs,ys) = getExts ul

instance SoundFileExts ULencode where
  getExts W = (".wav",".wav")
  getExts UL1 = (".wav",".ul")
  getExts UL0 = (".ul",".wav")
  getExts _ = (".ul",".ul")

-- | The variant of the 'soxOpG' that is used if the second file is not used (or in the situation where some
-- other file is used, too, e. g. with the .prof extension). For the functions in the module, this corresponds
-- to the \"-n\" second file argument.
soxOpG1 :: ULencode -> [String] -> FilePath -> [String] -> [String] -> IO (ExitCode, ByteString, ByteString)
soxOpG1 ul xss file1 yss zss
 | (fst . getExts $ ul) `isSuffixOf` file1 =
    if ul < UL0 then readProcess (proc (showE0Dup "sox") (filter (not . null) . mconcat $ [xss, [file1], yss, ["-n"], zss]))
    else readProcess (proc (showE0Dup "sox") (filter (not . null) . mconcat $ [xss, ulAccessParameters, [file1], yss, ["-n"], zss])) 
 | otherwise = error "Composition.Sound.SoX.soxOpG1: A given file has inappropriate file extension, or there has occurred some other error. Please, check the arguments. "

-- | Function 'durationAG' returns a duration of the audio file in seconds.
durationAG :: ULencode -> FilePath -> IO Float
durationAG ul file 
  | null . showE0Dup $ "soxi" = error "Composition.Sound.SoX.durationAG: The SoX executable is not properly installed."
  | otherwise = do
      (_, hout) <- readProcessStdout (proc (showE0Dup "soxi") (if ul < UL0 then ["-D",file] else mconcat [["-D"],ulAccessParameters,[file]]))
      let x0 = foldr (:) [] hout
      return (read x0::Float)

-- | A variant of the 'durationAG' with the first argument being 'W'.
durationA :: FilePath -> IO Float
durationA = durationAG W
{-# INLINE durationA #-}

-- | Function 'upperBndG' returns a maximum number of samples for use in other functions.
upperBndG :: ULencode -> FilePath -> IO Int
upperBndG ul file 
  | null . showE0Dup $ "soxi" = error "Composition.Sound.SoX.upperBndG: The SoX executable is not properly installed."
  | otherwise = do 
       (_, hout) <- readProcessStdout (proc (showE0Dup "soxi") (if ul < UL0 then ["-s",file] else mconcat [["-s"],ulAccessParameters,[file]])) 
       let x0 = foldr (:) [] hout
       return (read x0::Int) 

-- | A variant of the 'selMaxAbsG' with the first argument being 'W'.
selMaxAbs :: FilePath -> (Int, Int) -> IO (ByteString, Bool)
selMaxAbs = selMaxAbsG W
{-# INLINE selMaxAbs #-}

-- | Function 'maxAbs' allows to choose a maximum by absolute value if the values are written as 'ByteString'. Bool 'True' corresponds to maximum value, 'False' - to minimum value
maxAbs :: (ByteString, ByteString) -> (ByteString, Bool)
maxAbs ([], _) = ([], False)
maxAbs (_, []) = ([], False)
maxAbs (xs, ys) 
 | dropWhile (== '-')  xs > dropWhile (== '-') ys = (xs, True)
 | otherwise = (ys, True)
{-# INLINE maxAbs #-}

ulAccessParameters :: [String]
ulAccessParameters = ["-r22050","-c1"]
{-# INLINE ulAccessParameters #-}

