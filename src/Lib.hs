module Lib
    ( testDriver,
      dollars,
      parse,
      translate,
      validateInteger,
      translateInteger,
      validateSplits,
      pluralityDollars,
      pluralityCents,
      chunkCents,
      nonSystematic,
      chewTensAndUnits,
      chewThousandPower,
      chewHundredsTensAndUnits,
      chew,
      shortScaleThousandPowerNames
    ) where

import Text.Numeral.Grammar ( defaultInflection )
import Text.Numeral.Language.ENG (us_cardinal)
import Data.Text (Text, unpack)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (intercalate)

nonSystematic :: String -> String
nonSystematic "0" = "zero"
nonSystematic "1" = "one"
nonSystematic "2" = "two"
nonSystematic "3" = "three"
nonSystematic "4" = "four"
nonSystematic "5" = "five"
nonSystematic "6" = "six"
nonSystematic "7" = "seven"
nonSystematic "8" = "eight"
nonSystematic "9" = "nine"
nonSystematic "00" = "zero"
nonSystematic "01" = "one"
nonSystematic "02" = "two"
nonSystematic "03" = "three"
nonSystematic "04" = "four"
nonSystematic "05" = "five"
nonSystematic "06" = "six"
nonSystematic "07" = "seven"
nonSystematic "08" = "eight"
nonSystematic "09" = "nine"
nonSystematic "10" = "ten"
nonSystematic "11" = "eleven"
nonSystematic "12" = "twelve"
nonSystematic "13" = "thirteen"
nonSystematic "14" = "fourteen"
nonSystematic "15" = "fifteen"
nonSystematic "16" = "sixteen"
nonSystematic "17" = "seventeen"
nonSystematic "18" = "eighteen"
nonSystematic "19" = "nineteen"
nonSystematic _    = ""

systematicTens :: String -> String
systematicTens "2" = "twenty"
systematicTens "3" = "thirty"
systematicTens "4" = "forty"
systematicTens "5" = "fifty"
systematicTens "6" = "sixty"
systematicTens "7" = "seventy"
systematicTens "8" = "eighty"
systematicTens "9" = "ninety"
systematic _   = ""

-- Max number of digits accomodated by the list of short scale names below
-- See Wikipedia article on large numbers for names
maxLength = 39 :: Int

shortScaleThousandPowerNames :: [String]
shortScaleThousandPowerNames =
  ["", "thousand", "million", "billion", "trillion"
  , "quadrillion", "quintillion", "sextillion"
  , "septillion", "octillion", "nonillion", "decillion"
  , "undecillion"]

systematicHighers :: String -> String
systematicHighers "0" = ""
systematicHighers "1" = "one hundred"
systematicHighers "2" = "two hundred"
systematicHighers "3" = "three hundred"
systematicHighers "4" = "four hundred"
systematicHighers "5" = "five hundred"
systematicHighers "6" = "six hundred"
systematicHighers "7" = "seven hundred"
systematicHighers "8" = "eight hundred"
systematicHighers "9" = "nine hundred"
systematicHighers _   = ""

chewTensAndUnits :: String -> String
chewTensAndUnits tu@[high,low] =
  let nstu = nonSystematic tu
  in
    if nstu == ""
      then
        let ts = systematicTens [high]
            us = nonSystematic [low]
        in
          if us == "zero"
            then ts else
              if ts == ""
                then us
                else ts ++ "-" ++ us
      else nstu
chewTensAndUnits _ = ""

chewHundredsTensAndUnits :: String -> String
chewHundredsTensAndUnits tu@[high, middle, low] =
  let hs = systematicHighers [high]
      theRest = chewTensAndUnits [middle, low]
  in
    if theRest == "zero"
       then hs
       else hs ++ " and " ++ theRest
chewHundredsTensAndUnits _ = ""

chewThousandPowerHelper1 power [units] =
    let tpn = shortScaleThousandPowerNames !! power
        nsu = nonSystematic [units]
    in
     if nsu == "zero" && tpn /= ""
      then ("", True)
      else
        if tpn == ""
         then (nsu, True)
         else (nsu ++ " " ++ tpn, True)
chewThousandPowerHelper1 _ _ = ("", False)

chewThousandPowerHelper2 power [tens, units] =
     let tpn = shortScaleThousandPowerNames !! power
         nsu = chewTensAndUnits [tens,units]
     in
     if nsu == "zero" && tpn /= ""
      then ("", True)
      else
       if tpn == ""
        then (nsu, True)
        else (nsu ++ " " ++ tpn, True)
chewThousandPowerHelper2 _ _ = ("", False)

chewThousandPower :: Int -> String ->(String, Bool)
chewThousandPower power ['0', '0', units] = chewThousandPowerHelper1 power [units]
chewThousandPower power ['0', units] = chewThousandPowerHelper1 power [units]
chewThousandPower power [units] = chewThousandPowerHelper1 power [units]
chewThousandPower power ['0', tens, units] = chewThousandPowerHelper2 power [tens, units]
chewThousandPower power [tens, units] = chewThousandPowerHelper2 power [tens, units]
chewThousandPower power theTriple@[a,b,c] =
  let tpn = shortScaleThousandPowerNames !! power
      nsu = chewHundredsTensAndUnits theTriple
  in
  if nsu == "zero" && tpn /= ""
   then ("", True)
   else
    if tpn == ""
     then (nsu, False)
     else (nsu ++ " " ++ tpn, False)
chewThousandPower _ _ = ("", False)

chew :: Int -> String -> String -> Bool -> String
chew thousandPower [] outputString andPending = outputString
chew thousandPower reversedInputString outputString andPending =
  let chunks = splitAt 3 reversedInputString
      chunk = reverse (fst chunks)
      ongoingChunk = snd chunks
      translatedChunk = chewThousandPower thousandPower chunk
      newAndPending = snd translatedChunk
      str = fst translatedChunk
      next
          | str == "zero" && ongoingChunk /= "" = outputString
          | outputString == "" = str
          | str == "" = outputString
          | otherwise = str ++ " " ++ outputString
  in
    chew (thousandPower + 1)
        ongoingChunk
        next
        newAndPending

validateInteger :: String -> Bool
validateInteger s =
  dropWhile isDigit s == ""  && length s <= maxLength

validateSplits :: [String] -> Bool
validateSplits s =
  length s <= 2

translateInteger :: String -> String
translateInteger i = chew 0 (reverse i) [] False

plurality :: String -> String
plurality "one" = ""
plurality _ = "s"

pluralityDollars :: String -> String
pluralityDollars dollars =
  let ds = translateInteger dollars
  in  ds ++ " dollar" ++ plurality ds

pluralityCents :: String -> String
pluralityCents cents =
  let cs = chewTensAndUnits cents
  in  cs ++ " cent" ++ plurality cs

chunkCents :: String -> String
chunkCents cents
  | null cents        = "00"
  | length cents == 1 = cents ++ "0"
  | length cents == 2 = cents
  | otherwise         = take 2 cents

translate :: [String] -> String
translate [a] =
            if a == ""
                then "zero dollars and zero cents"
                else pluralityDollars a ++ " and zero cents"
translate [a, b]
  | a == "" && b == "" = ""
  | a == ""            = "zero dollars and " ++ pluralityCents (chunkCents b)
  | b == ""            = pluralityDollars a ++ " and zero cents"
  | otherwise          = pluralityDollars a ++ " and " ++ pluralityCents (chunkCents b)

parse :: String -> [String]
parse input =
  let subStrings = splitOn "." input
  in
    if validateSplits subStrings && validateInteger (head subStrings)
       && ((length subStrings /= 2) || validateInteger (head (tail subStrings)))
    then subStrings
    else [""]

dollars :: String -> String
dollars input =
  translate (parse input)

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
    -- Don't use dollar or cents values larger than 39 digits
    print (dollars "1000000000000000000000000000000000000000000" == "zero dollars and zero cents")
    print (dollars ".1000000000000000000000000000000000000000000" == "zero dollars and zero cents")
    print (dollars "100000000000000000000000000000000000000.0" == "one hundred undecillion dollars and zero cents")
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
    print (dollars "9123456.13" == "nine million one hundred and twenty-three thousand four hundred and fifty-six dollars and thirteen cents")

    putStrLn "testDriver end"
