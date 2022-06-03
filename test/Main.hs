import Horsep
import Test.Hspec
import Test.HUnit

main :: IO ()
main = hspec $ do
    describe "Parse elementary components" $ do
        it "Parse single dot" $ do
            run decodeDot "." @?= (MorseDot, "")
        it "Parse single dash" $ do
            run decodeDash "-" @?= (MorseDash, "")