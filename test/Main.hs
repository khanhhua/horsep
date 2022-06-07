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
      it "Parse space" $ do run decodeSilence " " @?= Just (MorseSilence, "")
    describe "Parse token" $ do
      it "Parse token '.'" $ do run decodeToken "." @?= Just (MorseDot, "")
      it "Parse token '.   '" $ do run decodeToken ".   " @?= Just (MorseDot, "   ")
      it "Parse token '-'" $ do run decodeToken "-" @?= Just (MorseDash, "")
      it "Parse token '-   '" $ do run decodeToken "-   " @?= Just (MorseDash, "   ")

    describe "Parse characters" $ do
      let
        jMorseChar = MorseChar
        jMorseInt = MorseInt

      -- it "Parse three silence into one nothing" $ do
      --   run decodeLetter "   " @?= Just (Nothing, "")
      it "Parse .-" $ do
        run decodeLetter ".-" @?= Just (jMorseChar 'A', "")
      it "Parse .-   " $ do
        run decodeLetter ".-   " @?= Just (jMorseChar 'A', "")
      it "Parse .-   .-" $ do
        run decodeLetter ".-   .-" @?= Just (jMorseChar 'A', ".-")
      it "Parse -..." $ do
        run decodeLetter "-..." @?= Just (jMorseChar 'B', "")
      it "Parse -.-." $ do
        run decodeLetter "-.-." @?= Just (jMorseChar 'C', "")
      it "Parse -.." $ do
        run decodeLetter "-.." @?= Just (jMorseChar 'D', "")
      it "Parse ." $ do
        run decodeLetter "." @?= Just (jMorseChar 'E', "")
      it "Parse ..-." $ do
        run decodeLetter "..-." @?= Just (jMorseChar 'F', "")
      it "Parse --." $ do
        run decodeLetter "--." @?= Just (jMorseChar 'G', "")
      it "Parse ...." $ do
        run decodeLetter "...." @?= Just (jMorseChar 'H', "")
      it "Parse .." $ do
        run decodeLetter ".." @?= Just (jMorseChar 'I', "")
      it "Parse .---" $ do
        run decodeLetter ".---" @?= Just (jMorseChar 'J', "")
      it "Parse -.-" $ do
        run decodeLetter "-.-" @?= Just (jMorseChar 'K', "")
      it "Parse .-.." $ do
        run decodeLetter ".-.." @?= Just (jMorseChar 'L', "")
      it "Parse --" $ do
        run decodeLetter "--" @?= Just (jMorseChar 'M', "")
      it "Parse -." $ do
        run decodeLetter "-." @?= Just (jMorseChar 'N', "")
      it "Parse ---" $ do
        run decodeLetter "---" @?= Just (jMorseChar 'O', "")
      it "Parse .--." $ do
        run decodeLetter ".--." @?= Just (jMorseChar 'P', "")
      it "Parse --.-" $ do
        run decodeLetter "--.-" @?= Just (jMorseChar 'Q', "")
      it "Parse .-." $ do
        run decodeLetter ".-." @?= Just (jMorseChar 'R', "")
      it "Parse ..." $ do
        run decodeLetter "..." @?= Just (jMorseChar 'S', "")
      it "Parse -" $ do
        run decodeLetter "-" @?= Just (jMorseChar 'T', "")
      it "Parse ..-" $ do
        run decodeLetter "..-" @?= Just (jMorseChar 'U', "")
      it "Parse ...-" $ do
        run decodeLetter "...-" @?= Just (jMorseChar 'V', "")
      it "Parse .--" $ do
        run decodeLetter ".--" @?= Just (jMorseChar 'W', "")
      it "Parse -..-" $ do
        run decodeLetter "-..-" @?= Just (jMorseChar 'X', "")
      it "Parse -.--" $ do
        run decodeLetter "-.--" @?= Just (jMorseChar 'Y', "")
      it "Parse --.." $ do
        run decodeLetter "--.." @?= Just (jMorseChar 'Z', "")
      it "Parse -----" $ do
        run decodeLetter "-----" @?= Just (jMorseInt 0, "")
      it "Parse .----" $ do
        run decodeLetter ".----" @?= Just (jMorseInt 1, "")
      it "Parse ..---" $ do
        run decodeLetter "..---" @?= Just (jMorseInt 2, "")
      it "Parse ...--" $ do
        run decodeLetter "...--" @?= Just (jMorseInt 3, "")
      it "Parse ....-" $ do
        run decodeLetter "....-" @?= Just (jMorseInt 4, "")
      it "Parse ....." $ do
        run decodeLetter "....." @?= Just (jMorseInt 5, "")
      it "Parse -...." $ do
        run decodeLetter "-...." @?= Just (jMorseInt 6, "")
      it "Parse --..." $ do
        run decodeLetter "--..." @?= Just (jMorseInt 7, "")
      it "Parse ---.." $ do
        run decodeLetter "---.." @?= Just (jMorseInt 8, "")
      it "Parse ----." $ do
        run decodeLetter "----." @?= Just (jMorseInt 9, "")
      it "Parse ''" $ do
        run decodeLetter "" @?= Nothing

    describe "Parse words" $ do
      it "Parse .-" $ do run decodeWord ".-" @?= Just (MorseWord "A", "")
      it "Parse .-   .-" $ do run decodeWord ".-   .-" @?= Just (MorseWord "AA", "")
      it "Parse .-   -...   -.-." $ do run decodeWord ".-   -...   -.-." @?= Just (MorseWord "ABC", "")
    
    describe "Parse sentence" $ do
      it "Parse ....   ..       .-   -...   -.-." $ do run decode "....   ..       .-   -...   -.-." @?= Just ([MorseWord "HI", MorseWord "ABC"], "")
    
    describe "Decode sentence to string" $ do
      it "Parse ....   ..       .-   -...   -.-." $ do runT "....   ..       .-   -...   -.-." @?= "HI ABC"
    -- describe "Parse unhappy cases" $ do
    --   it "Parse invalid series of token" $ do
    --     run decodeLetter "........" @?= Just Nothing