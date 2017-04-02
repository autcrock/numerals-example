module Lib
    ( someFunc
    ) where

import Text.Numeral.Grammar ( defaultInflection )
import Text.Numeral.Language.ENG (gb_cardinal, us_cardinal)
import Data.Text (Text, unpack)

liftText :: Maybe Text -> String
liftText (Just t) = unpack t
liftText _ = error "Bad Conversion"

someFunc :: IO ()
someFunc =
  do
    putStrLn "someFunc start"
    putStrLn $ liftText (gb_cardinal defaultInflection 123 :: Maybe Text)
    putStrLn $ liftText (us_cardinal defaultInflection 123 :: Maybe Text)
    putStrLn $ liftText (gb_cardinal defaultInflection (10^50 + 42) :: Maybe Text)
    putStrLn $ liftText (us_cardinal defaultInflection (10^50 + 42) :: Maybe Text)
    --    Just "one hundred quindecillion forty-two"
    putStrLn "someFunc end"
