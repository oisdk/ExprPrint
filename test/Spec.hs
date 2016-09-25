import Test.DocTest

main :: IO ()
main = doctest [ "-isrc", "src/Data/Text/ExprPrint.hs" ]
