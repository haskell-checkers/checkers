module Test.QuickCheck.Instances.Char
       (nonSpace,whitespace,space,newline
       ,lowerAlpha,upperAlpha,numeric
       ,parenthesis,bracket,brace
       ,operator
       ) where

import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Help
import Control.Monad

instance Arbitrary Char where
    arbitrary     = choose ('\0','\255')
    coarbitrary c = variant (ord c `rem` 4)

{- | Generates a 'non space' character, i.e. any ascii except
     ' ', '\t', '\n' and '\r'.
-}
nonSpace :: Gen Char
nonSpace = notOneof " \t\n\r"

{- | Generates any whitespace character, including new lines.
-}
whitespace :: Gen Char
whitespace = oneof [space,newline]

{- | Generates a whitespace charecter, not a newline.
-}
space :: Gen Char
space = oneof (map return " \t")

{- | Generates either a '\n' or '\r'.
-}
newline :: Gen Char
newline = oneof (map return "\n\r")

letters :: String
letters = "abcdefghijklmnopqrstuvwxyz"

{- | Generates any lower case alpha character.
-}
lowerAlpha :: Gen Char
lowerAlpha = oneof (map return letters)

{- | Generates any upper case alpha character.
-}
upperAlpha :: Gen Char
upperAlpha = oneof (map (return . toUpper) letters)

{- | Generates a digit character.
-}
numeric :: Gen Char
numeric = oneof (map return "1234567890")

{- | Generates one or other of '(' and ')'.
-}
parenthesis :: Gen Char
parenthesis = oneof (map return "()")

{- | Generates one or other of '[' and ']'.
-}
bracket :: Gen Char
bracket = oneof (map return "[]")

{- | Generates one or other of '{' and '}'.
-}
brace :: Gen Char
brace = oneof (map return "{}")

{- | Generates one of '*', '/', '-', '+', '<', '>', '|' and '#'.
-}
operator :: Gen Char
operator = oneof (map return "*/-+<>|#")
