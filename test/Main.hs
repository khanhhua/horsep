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
      it "Parse ..-." $ do
        run decodeLetter "..-." @?= Just (MorseChar 'F', "")
      it "Parse --." $ do
        run decodeLetter "--." @?= Just (MorseChar 'G', "")
      it "Parse ...." $ do
        run decodeLetter "...." @?= Just (MorseChar 'H', "")
      it "Parse .." $ do
        run decodeLetter ".." @?= Just (MorseChar 'I', "")
      it "Parse .---" $ do
        run decodeLetter ".---" @?= Just (MorseChar 'J', "")
      it "Parse -.-" $ do
        run decodeLetter "-.-" @?= Just (MorseChar 'K', "")
      it "Parse .-.." $ do
        run decodeLetter ".-.." @?= Just (MorseChar 'L', "")
      it "Parse --" $ do
        run decodeLetter "--" @?= Just (MorseChar 'M', "")
      it "Parse -." $ do
        run decodeLetter "-." @?= Just (MorseChar 'N', "")
      it "Parse ---" $ do
        run decodeLetter "---" @?= Just (MorseChar 'O', "")
      it "Parse .--." $ do
        run decodeLetter ".--." @?= Just (MorseChar 'P', "")
      it "Parse --.-" $ do
        run decodeLetter "--.-" @?= Just (MorseChar 'Q', "")
      it "Parse .-." $ do
        run decodeLetter ".-." @?= Just (MorseChar 'R', "")
      it "Parse ..." $ do
        run decodeLetter "..." @?= Just (MorseChar 'S', "")
      it "Parse -" $ do
        run decodeLetter "-" @?= Just (MorseChar 'T', "")
      it "Parse ..-" $ do
        run decodeLetter "..-" @?= Just (MorseChar 'U', "")
      it "Parse ...-" $ do
        run decodeLetter "...-" @?= Just (MorseChar 'V', "")
      it "Parse .--" $ do
        run decodeLetter ".--" @?= Just (MorseChar 'W', "")
      it "Parse -..-" $ do
        run decodeLetter "-..-" @?= Just (MorseChar 'X', "")
      it "Parse -.--" $ do
        run decodeLetter "-.--" @?= Just (MorseChar 'Y', "")
      it "Parse --.." $ do
        run decodeLetter "--.." @?= Just (MorseChar 'Z', "")
      it "Parse -----" $ do
        run decodeLetter "-----" @?= Just (MorseInt 0, "")
      it "Parse .----" $ do
        run decodeLetter ".----" @?= Just (MorseInt 1, "")
      it "Parse ..---" $ do
        run decodeLetter "..---" @?= Just (MorseInt 2, "")
      it "Parse ...--" $ do
        run decodeLetter "...--" @?= Just (MorseInt 3, "")
      it "Parse ....-" $ do
        run decodeLetter "....-" @?= Just (MorseInt 4, "")
      it "Parse ....." $ do
        run decodeLetter "....." @?= Just (MorseInt 5, "")
      it "Parse -...." $ do
        run decodeLetter "-...." @?= Just (MorseInt 6, "")
      it "Parse --..." $ do
        run decodeLetter "--..." @?= Just (MorseInt 7, "")
      it "Parse ---.." $ do
        run decodeLetter "---.." @?= Just (MorseInt 8, "")
      it "Parse ----." $ do
        run decodeLetter "----." @?= Just (MorseInt 9, "")
    -- describe "Parse unhappy cases" $ do
    --   it "Parse invalid series of token" $ do
    --     run decodeLetter "........" @?= Just Nothing