import Horsep
import Test.HUnit
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Parse elementary components" $ do
      it "Parse single dot" $ do run decodeDot "." @?= (Just MorseDot, "")
      it "Parse single dash" $ do run decodeDash "-" @?= (Just MorseDash, "")
      it "Parse space" $ do run decodeSpace " " @?= (Just MorseSpace, "")
    describe "Parse token" $ do
      it "Parse token '.'" $ do run decodeToken ". " @?= (Just MorseDot, " ")
      it "Parse token '-'" $ do run decodeToken "- " @?= (Just MorseDash, " ")
      it "Parse token ' '" $ do run decodeToken "  " @?= (Just MorseSpace, " ")
    -- describe "Parse characters" $ do
    --   it "Parse ." $ do
    --       run decodeLetter "." @?= (Just $ MorseChar 'i', "")
