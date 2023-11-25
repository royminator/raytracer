import Test.Tasty
import MathTests (mathTests)

main :: IO ()
main = defaultMain mathTests
