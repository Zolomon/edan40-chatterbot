module Chatterbot where
import Utilities
import Pattern
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

-- Takes a brain, and returns a function that takes phrase and returns a random response.
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = 
  do 
    r <- randomIO :: IO Float
    (\sentence -> rulesApply ()
  -- [([String], [[String]])] rulesApply (concatMap (\(x, xs) -> map ((,) x) xs) $ rulesCompile eliza) ["Why", "don't", "you", "fuck", "me", "?"]
--stateOfMind _ = return id

-- Returns a function that takes a Phrase (List of strings (words)) and returns the lookedup phrase in some dictionary, and applies reflect on the intermediate result before returning it. It applies a rule to a lookedup value.
rulesApply :: [PhrasePair] -> Phrase -> Phrase
-- rulesApply xs = try (transformationsApply "*" (reflect) xs)
rulesApply dictionary sentence = try (transformationsApply "*" (reflect) dictionary) sentence

-- Takes a phrase and returns a reflect phrase
reflect :: Phrase -> Phrase
reflect = map search
  where search = try (\word -> lookup word reflections)

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

-- Takes eliza structure and converts it to a bot brain
rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile [] = []
rulesCompile (x:xs) = (words $ fst x, map (prepare) (snd x)) : rulesCompile xs
--rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


