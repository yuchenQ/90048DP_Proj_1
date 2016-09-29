module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List (delete, sort, minimumBy)
import Data.Function (on)
import qualified Data.Map as Map (fold, fromListWith)
-- The current state of the game in anytime, which is a list of lists of cards,
-- which are the underlying answer
data GameState = GameState {potentialAnswer :: [[Card]]} deriving (Show)
-- =================================================================================================
-- Fuction: feedback
-- =================================================================================================
-- Takes a guess and a list of cards (as answer) and shows the feedback,
-- which shown as a quintuple, will be returned
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback _ [] = error "Error happens"
feedback [] _ = error "Error happens"
feedback answer guess = (all_right, num_lower, rank_right, num_higher, suit_right)
  where all_right = find_Correct guess answer
        -- num_lower = find_Numlower (lowest_Rank guess) answer
        num_lower = find_Numlower guess answer
        rank_right = num_Rankright (sort $ map rank guess) (sort $ map rank answer)
        -- num_lower = find_Numhigher (highest_Rank guess) answer
        num_higher = find_Numhigher guess answer
        suit_right = num_Suitright (sort $ map suit guess) (sort $ map suit answer)
----------------------------------------------------------------------------------------------------
-- Auxillary Functions
----------------------------------------------------------------------------------------------------
-- Takes the guess and answer
-- Returns number of cards that are in both the guess and the answer
find_Correct :: [Card] -> [Card] -> Int
find_Correct guess answer = length (filter (`elem` answer) guess)
----------------------------------------------------------------------------------------------------
-- Takes a guess and an answer
-- Returns how many cards in the answer have a rank lower than the lowest rank in the guess

-- find_Numlower :: Rank -> [Card] -> Int
-- find_Numlower lowest answer =
--     length $ filter (\x -> rank x < lowest) answer
--
-- lowest_Rank :: [Card] -> Rank
-- lowest_Rank guess = head $ sort $ map rank guess

-- The code shown above is sepreate version of my code, as reference:
find_Numlower :: [Card] -> [Card] -> Int
find_Numlower guess answer = length $ filter (\x -> rank x < (head $ sort $ map rank guess)) answer
----------------------------------------------------------------------------------------------------
-- Takes a guess rank and an answer rank
-- Returns how many cards in the answer have the same rank as a card in the guess
num_Rankright :: [Rank] -> [Rank] -> Int
num_Rankright _ [] = 0
num_Rankright [] _ = 0
num_Rankright (t:ts) (a:as)
  | t == a = 1 + num_Rankright ts as
  | t > a = num_Rankright (t:ts) as
  | t < a = num_Rankright ts (a:as)
----------------------------------------------------------------------------------------------------
-- Takes a guess and an answer
-- Returns how many cards in the answer have a rank higher than the highest rank in the guess

-- find_Numhigher :: Rank -> [Card] -> Int
-- find_Numhigher highest answer =
--     length $ filter (\x -> rank x > highest) answer
--
-- highest_Rank :: [Card] -> Rank
-- highest_Rank guess = head $ sort $ map rank guess

-- The code shown above is sepreate version of my code, as reference:
find_Numhigher :: [Card] -> [Card] -> Int
find_Numhigher guess answer = length $ filter (\x -> rank x > (last $ sort $ map rank guess)) answer
----------------------------------------------------------------------------------------------------
-- Takes a guess suit and an answer suit
-- Returns how many cards in the answer have the same suit as a card in the guess
num_Suitright :: [Suit] -> [Suit] -> Int
num_Suitright _ [] = 0
num_Suitright [] _ = 0
num_Suitright (t:ts) (a:as)
  | t == a = 1 + num_Suitright ts as
  | t > a = num_Suitright (t:ts) as
  | t < a = num_Suitright ts (a:as)
-- =================================================================================================
-- Fuction: initialGuess
-- =================================================================================================
-- Takes some cards(2 to 4),then computes an initial guess
-- Each card inside the first guess will have different card compare to others
-- For n cards, choose ranks that are about 13/(n+1) ranks apart in first guess
initialGuess :: Int -> ([Card],GameState)
initialGuess 0 = error "n must be larger than 0"
initialGuess n = (guess, GameState xs)
  where guess = initCards n increment increment 0
        xs = delete guess all_answer
        increment = (13 :: Double) / fromIntegral (n+1)
        all_answer = generateAns n all_cards
        all_cards = [minBound..maxBound] :: [Card]
----------------------------------------------------------------------------------------------------
-- Auxillary Functions
----------------------------------------------------------------------------------------------------
-- Takes n number of cards, current rank of set, increment, and current suit for the current card
-- Each card inside the first guess will have different card compare to others
-- For n cards, choose ranks that are about 13/(n+1) ranks apart in first guess
-- Returns list of cards as initial guess.
initCards :: (RealFrac a) => Int -> a -> a -> Int -> [Card]
initCards 0 _ _ _ = []
initCards n rank increment suit
  | n < 0 = error "n must be positive"
  | otherwise = Card (getSuit suit_index) (getRank $ floor rank) :
                      initCards (n - 1) (rank + increment) increment (suit + 1)
                        where suit_index = if suit > 3 then 0 else suit
-- Takes integer index for a suit
-- Returns suit corresponding to that integer index
getSuit :: Int -> Suit
getSuit n = [Club .. Spade] !! n
-- Takes integer index for a rank
-- Returns rank corresponding to that integer index
getRank :: Int -> Rank
getRank n = [R2 .. Ace] !! n
----------------------------------------------------------------------------------------------------
-- Takes number of cards will be set as the type, the all cards and suits
-- Create all the potential answers, which is a list of lists of cards
generateAns :: Int -> [Card] -> [[Card]]
generateAns 0 _ = []
generateAns _ [] = []
generateAns _ [_] = []
generateAns 1 x = map (:[]) x
generateAns n (x:xs) = [x:y | y <- generateAns (n-1) xs] ++ generateAns n xs
-- =================================================================================================
-- Fuction: nextGuess
-- =================================================================================================
-- Takes a tuple containing the previous guess and game state and the feedback from previous guess
-- Computes the next guess should be and then, update the gamestate
-- Returns next guess and new game state
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (last_guess, GameState gs) feed_back = (new_guess, GameState new_gs)
  where
    -- limit the number of potential cards combinations which will be calculated,
    -- for reduce time consuming
    new_guess = selectNextguess (if length (x:xs) < 500 then x:xs else take 500 (x:xs))
    (x:xs) = filter (\combo -> feedback combo last_guess == feed_back) gs
    new_gs = delete new_guess (x:xs)
----------------------------------------------------------------------------------------------------
-- Auxillary Functions
----------------------------------------------------------------------------------------------------
-- Take a list of lists of cards, then filter it by using the expected number of
-- remaining possible answers, which set as the benchmark
-- Return the one combination of cards which is the smallest one of remaining possible answers
selectNextguess :: [[Card]] -> [Card]
selectNextguess gs =
  let mini_one = minimumBy (compare `on` snd) possible_ans
  in fst mini_one
    where
      possible_ans = [(guess, remainAns) | guess <- gs,
                          let remainAns = weightedValue guess gs]
----------------------------------------------------------------------------------------------------
-- Takes a guess and a list of underlying answers as input
-- get the map which key is feedback, and the frequency of the feedback as content
-- then calculate the expect number for each segement by the formula
-- Returns expected number of remaining possible answers, which will be set as 
-- the sum of the squares of the group sizes divded by sum of group sizes
weightedValue :: [Card] -> [[Card]] -> Double
weightedValue guess gs = fromIntegral sum_squares / fromIntegral sum_length
  where
    sum_squares = Map.fold (\x y -> x*x + y) 0 fb_frequency
    sum_length  = Map.fold (+) 0 fb_frequency
    fb_frequency = Map.fromListWith (+) [(feedback combo guess, 1) | combo <- gs]
----------------------------------------------------------------------------------------------------
-- alternative solution of nextGuess:

-- just extract some which have the same feedback as the feedback of previous guess and answer,
-- then set new guess is the first of them, set the next gamestate as the rest of them.
-- simple but effect, even faster than the version I provided.

-- nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
-- nextGuess (last_guess, GameState gs) fb = (new_guess, GameState new_gs)
--   where (x:xs) = filter (\combo -> feedback combo last_guess == fb) gs
--         new_guess = x
--         new_gs = xs
