import Lib
import Test.QuickCheck

main :: IO ()
main = do --unit tests
    putStrLn ""
    putStrLn $ if readExpr "#" == "Found value" then "OK" else "FAIL"
    return ()
