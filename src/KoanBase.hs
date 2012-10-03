{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module HaskellKoans.KoanBase
(
    (???),
    Koans(..),
    Koan(..),
    KoanResult,
    assertBool,
    assertEqual,
    doKoans
) where

import Control.Exception as E

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

runKoan :: Koan -> IO (String, KoanResult)
runKoan (Koan name assert) = fmap (name,) assert

runKoans :: Koans -> (String, IO [(String, KoanResult)])
runKoans (Koans name koans) = (name, mapM runKoan koans)

doKoans :: Koans -> IO ()
doKoans koans = do
    let (name, runResult) = runKoans koans
    results <- runResult
    putStrLn $ "Koans: " ++ name
    mapM_ (\(s, r) -> putStrLn . show $ r) results


--takeUpTo :: (a -> Bool) -> [a] -> [a]
--takeUpTo pred list = fst . takeWhile (\(_, y) -> pred y) $ zip list (drop 1 list)
