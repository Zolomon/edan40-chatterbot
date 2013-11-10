module Pattern where
import Utilities
import Data.Maybe
-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (x:xs) s 
  | w == x = s ++ substitute w xs s
  | otherwise = x : substitute w xs s
                
-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _  [] = Nothing
match _ [] _ = Nothing
match wc (p:ps) (s:ss)
  | wc /= p = if p == s 
              then match wc ps ss
              else Nothing
  | otherwise = longerWildcardMatch (p:ps) (s:ss) `orElse` singleWildcardMatch (p:ps) (s:ss)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch [] [] = Just []
singleWildcardMatch _ []  = Nothing
singleWildcardMatch [] _  = Nothing
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)

longerWildcardMatch [] [] = Just []
longerWildcardMatch _ [] = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:ps) (x:xs) = mmap (x :) (match wc (wc:ps) xs)


-- Test cases --------------------

testPattern = "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard func lookupString dictionary = mmap (func . substitute wildcard (snd dictionary)) $ match wildcard (fst dictionary) lookupString

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wildcard func dictionary lookupString = foldr t Nothing dictionary
  where t x _ = transformationApply wildcard func lookupString x