{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module HaskellKoans.KoanBase
(
    (???),
    Koans(..),
    Koan(..),
    KoanResult,
    assertBool,
    assertEqual,
    runKoan, runKoans,
    doKoans
) where

import Control.Exception as E
import Control.Applicative
import Control.Monad

(???) :: a
(???) = undefined

type KoanSetName = String
data Koans = Koans KoanSetName [Koan]
data Koan = Koan String KoanAssert
type KoanAssert = IO KoanResult
data KoanResult = Pass String | Fail String deriving Show

assertBool :: String -> Bool -> KoanAssert
assertBool msg = assertEqual msg True

assertEqual :: (Eq a, Show a) => String -> a -> a -> KoanAssert
assertEqual msg expected actual =
    do 
        result <- tryJust errorCalls (actual `seq` return $ check (actual == expected)) 
        case result of
            Left _ -> return . Fail $ format (show expected) "(???)"
            Right v -> return v
    where check True = Pass $ "Answer: " ++ show expected
          check False = Fail $ format (show expected) (show actual)
          format expectedStr actualStr = "Expected: " ++ expectedStr ++ "\nActual: " ++ actualStr
          errorCalls (_ :: ErrorCall) = Just ()

--runKoans :: Koans -> IO [(Koan, KoanResult)]
--runKoans (Koans name koans) = do
--    putStrLn $ "Koans: " ++ name
--    ks <- sequence koans
--
isPass :: KoanResult -> Bool
isPass (Pass _) = True
isPass (Fail _) = False

runKoan :: Koan -> IO Bool
runKoan (Koan name assert) = do
    putStrLn name
    result <- assert
    print result
    return . isPass $ result

--runKoans :: Koans -> IO ()
runKoans (Koans name koans) = do
    putStrLn $ "Koans: " ++ name
    runUntilFail koans

runUntilFail :: [Koan] -> IO ()
runUntilFail [] = return ()
runUntilFail (x:xs) = do
    continue <- runKoan x
    when continue $ runUntilFail xs

doKoans = undefined

--takeUpTo :: (a -> Bool) -> [a] -> [a]
--takeUpTo pred list = fst . takeWhile (\(_, y) -> pred y) $ zip list (drop 1 list)
