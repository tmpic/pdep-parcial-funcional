module Library where
import PdePreludat
import Data.Char
import Data.Foldable

-- isPalindrome :: String -> Bool
-- isPalindrome [] = False
-- isPalindrome word = word == reverse word

isPalindrome :: String -> Maybe Bool
isPalindrome word =    
    case word of
        "" -> Nothing
        _ -> Just ((withoutNonAlpha . withoutSpaces . mapToLower) word == (withoutNonAlpha . withoutSpaces . mapToLower . reverse) word)

foo :: String -> String
foo word =
    case (isPalindrome word) of 
        Nothing -> "say what again"
        Just False -> "This is not a palindrome"
        Just True -> "This is a palindrome"

mapToLower :: String -> String
mapToLower word = map (toLower) word

withoutSpaces :: String -> String
withoutSpaces = filter (not . isSpace)

withoutNonAlpha :: String -> String
withoutNonAlpha = filter isAlpha

-- isSpace :: Char -> Bool
-- isSpace = (' ' ==) 

main :: IO ()
main = 
    do
        word <- getLine
        print (foo word)


example1 :: Maybe String
example1 = Nothing

example2 :: Maybe String
example2 = Just "Carlos"

c*(aa*bc*b*c*aa*c*bb*(a + c)* + bc*b*aa*c*bb*(a + c)a*c*bc*b*aa*c*bb*(a + c)* + ε)

1 = a*(b(c*aa*c)*c*aa*bb)*((c*aa*c)*c*aa*ba1 + a*b(c*aa*c)*c*aa*bc1 + a*b(c*aa*c)*c*b0) + a*c0

1 = (a*(b(c*aa*c)*c*aa*bb)*((c*aa*c)*c*aa*ba)* + (a*b(c*aa*c)*c*aa*bc)* + a*b(c*aa*c)*c*b0) + a*c0

1 = a*(b(c*aa*c)*c*aa*bb)*(((c*aa*c)*c*aa*ba + a*b(c*aa*c)*c*aa*bc) 1 + a*b(c*aa*c)*c*b0) + a*c0