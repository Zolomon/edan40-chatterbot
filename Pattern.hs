module Pattern where
import Utilities

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
match _ [] [] = Nothing
match _ _  [] = Nothing
match _ [] _ = Nothing
-- match w (p:ps) (s:ss) 
--   | p == s = match w ps ss
--   | w == p = 
-- match _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (_:ps) (x:xs) = 
   if all (== True) $ zipWith (==) ps xs  
   then Just [x]
   else Nothing

longerWildcardMatch _ [] = Just []
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:ps) (x:xs)  
  | wc == x = longerWildcardMatch ps xs
  | otherwise = Just xs

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
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}

