import Horsep
import Test.HUnit
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    -- describe "Parse empty string" $ do
    --   it "should be Eof" $ do run decode
    describe "Parse elementary components" $ do
      it "Parse single dot" $ do run decodeDot "." @?= Just (MorseDot, "")
      it "Parse single dash" $ do run decodeDash "-" @?= Just (MorseDash, "")
      it "Parse space" $ do run decodeSpace " " @?= Just (MorseSpace, "")
    describe "Parse token" $ do
      it "Parse token '.'" $ do run decodeToken ". " @?= Just (MorseDot, " ")
      it "Parse token '-'" $ do run decodeToken "- " @?= Just (MorseDash, " ")

    describe "Parse characters" $ do
      it "Parse .-" $ do
        run decodeLetter ".-" @?= Just (MorseChar 'A', "")
      it "Parse -..." $ do
        run decodeLetter "-..." @?= Just (MorseChar 'B', "")
      it "Parse -.-." $ do
        run decodeLetter "-.-." @?= Just (MorseChar 'C', "")
      it "Parse -.." $ do
        run decodeLetter "-.." @?= Just (MorseChar 'D', "")
      it "Parse ." $ do
        run decodeLetter "." @?= Just (MorseChar 'E', "")
