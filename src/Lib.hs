module Lib
    ( testDriver,
      dollars,
      parse,
      translate,
      validateInteger,
      validateSplits,
      liftText,
      pluralityDollars,
      pluralityCents,
      chunkCents,
      fixHundreds
    ) where

import Text.Numeral.Grammar ( defaultInflection )
import Text.Numeral.Language.ENG (us_cardinal)
import Data.Text (Text, unpack)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (intercalate)

liftText :: Maybe Text -> String
liftText (Just t) = unpack t
--liftText Nothing = error "dollars: liftText: Bad conversion from Text to String"
liftText Nothing = ""

validateInteger :: String -> Bool
validateInteger s =
  dropWhile isDigit s == ""

validateSplits :: [String] -> Bool
validateSplits s =
  length s <= 2

translateInteger :: Integer -> String
translateInteger i = liftText (us_cardinal defaultInflection i :: Maybe Text)

plurality :: String -> String
plurality "one" = ""
plurality _ = "s"

pluralityDollars :: Integer -> String
pluralityDollars dollars =
  let ds = translateInteger dollars
  in  ds ++ " dollar" ++ plurality ds

pluralityCents :: Integer -> String
pluralityCents cents =
  let cs = translateInteger cents
  in  cs ++ " cent" ++ plurality cs

fixHundreds :: String -> String
fixHundreds s =
  let hundredBreaks = splitOn "hundred" s
  in
    if length hundredBreaks == 1
      then s
      else
        let revBreaks = reverse hundredBreaks
            tensEtc = head revBreaks
            andTensEtc = " and" ++ tensEtc
            newRevBreaks = andTensEtc : tail revBreaks
            newHundredBreaks = reverse newRevBreaks
        in
          intercalate "hundred" newHundredBreaks

chunkCents :: String -> String
chunkCents cents
  | length cents == 0 = "00"
  | length cents == 1 = cents ++ "0"
  | length cents == 2 = cents
  | otherwise         = take 2 cents

translate :: [String] -> String
translate [a] =
            if a == ""
                then "zero dollars and zero cents"
                else pluralityDollars (read a) ++ " and zero cents"
translate [a, b]
--  | a == "" && b == "" = error "dollars: translate: Empty input"
  | a == "" && b == "" = ""
  | a == ""            = "zero dollars and " ++ pluralityCents (read $ chunkCents b)
  | b == ""            = pluralityDollars (read a) ++ " and zero cents"
  | otherwise          = pluralityDollars (read a) ++ " and " ++ pluralityCents (read $ chunkCents b)

parse :: String -> [String]
parse input =
  let subStrings = splitOn "." input
  in
    if validateSplits subStrings && validateInteger (head subStrings)
       && ((length subStrings /= 2) || validateInteger (head (tail subStrings)))
    then subStrings
--    else error ("dollars: parse: Invalid input: " ++ input)
    else [""]

dollars :: String -> String
dollars input =
  fixHundreds(translate (parse input))

testDriver :: IO ()
testDriver =
  do
    putStrLn "testDriver start"
    --
    --
    -- Some other example input and output:

    -- Invalid characters should be ignored, meaning that every input string has an output string.
    print (dollars "1.2.3" == "zero dollars and zero cents")
    print (dollars "a" == "zero dollars and zero cents")

    -- The empty string produces "zero dollars and zero cents"
    print (dollars "" == "zero dollars and zero cents")
    -- For example, the input "1.11" will result in a return value of "one dollar and eleven cents"
    print (dollars "1.11" == "one dollar and eleven cents")
    print (dollars "0" == "zero dollars and zero cents")
    print (dollars "1" == "one dollar and zero cents")
    print (dollars "0.1" == "zero dollars and ten cents")
    print (dollars "1." == "one dollar and zero cents")
    print (dollars "0." == "zero dollars and zero cents")
    print (dollars ".34" == "zero dollars and thirty-four cents")
    print (dollars "0.3456789" == "zero dollars and thirty-four cents")
    print (dollars "1.0" == "one dollar and zero cents")
    print (dollars "1.01" == "one dollar and one cent")
    print (dollars "1000456.13" == "one million four hundred and fifty-six dollars and thirteen cents")

    putStrLn "testDriver end"
