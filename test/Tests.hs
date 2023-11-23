import Test.Tasty
import DomainTests (domainTests)

main :: IO ()
main = defaultMain domainTests
