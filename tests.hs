-- file: tests.hs
-- Runs some tests for the Chatterbot
import Pattern

main :: IO ()
main = do
  print $ map (uncurry singleWildcardMatch) [("*do", "bdo"),
                                             ("b*o", "bdo"),
                                             ("bd*", "bdo")]