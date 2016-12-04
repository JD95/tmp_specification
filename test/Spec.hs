import Tests.List
import Tests.Instantiation

printResult (Right r) = print r
printResult (Left l) = print l

main :: IO ()
main = do
  instantiationTests
  listTests
