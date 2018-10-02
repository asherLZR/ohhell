module Player (
    playCard,
    makeBid
)
where
{-
Write a report describing your design and strategy here.

Perform a filter operation on the cards in hand to see if a suit can be followed.

How to deal with no cards returned in base case?

 a = [(Card Spade Two), (Card Diamond Two), (Card Heart Ace), (Card Club Two), (Card Heart Two)]
 bids = [("1", 10), ("2", 20), ("3", 30)]
-}

import OhTypes
import OhHell
import Data.List

-- From OhHell.hs
leadSuit :: Trick -> Suit
leadSuit c = let (Card suit _, _) = last c in suit

-- Filter the cards in hand based on the given Suit and sort them by Rank.
-- FIXME
cardsOfSuit :: [Card] -> Suit -> Bool -> [Card]
cardsOfSuit cds st inv = sort $ filter isSuit cds
    where isSuit (Card s _)
            | inv == False = if s == st then True else False
            | otherwise = if s == st then False else True

-- Filter the list of bids to find the bid of a player.
bidOfPlayer :: PlayerId -> [(PlayerId, Int)] -> Int
bidOfPlayer pId bids = let (x:_) = filter isPlayer bids
    in snd $ x
    where isPlayer bd = if fst bd == pId then True else False

-- Order the cards by trump Suit, lead Suit, ordered by Rank
orderCards :: [Card] -> Suit -> Suit -> [Card]
orderCards cds tmpSuit ldSuit 
    | tmpSuit == ldSuit = tmpCards ++ rest
    | otherwise = tmpCards ++ ldCards ++ rest
    where tmpCards = sort $ cardsOfSuit cds tmpSuit False
          ldCards = sort $ cardsOfSuit cds ldSuit False
          rest = sort $ cardsOfSuit (cardsOfSuit cds tmpSuit True) ldSuit True

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard pId cds bids tmp txs tx
    -- if we need to win more tricks, play the highest card
    | wonBds < bidOfPlayer pId bids = Card (leadSuit tx) Ace
    -- if we need to lose, play the lowest card
    | wonBds >= bidOfPlayer pId bids = Card (leadSuit tx) Three
        where (Card tmpSuit _) = tmp
              wonBds = foldr (const(+1)) 0 $ filter (\x -> if x == pId then True else False) $ map (winner tmpSuit) txs

-- type PlayFunc
--   =  PlayerId     -- ^ this player's Id so they can identify themselves in the bids and tricks
--   -> [Card]       -- ^ the player's cards
--   -> [(PlayerId,Int)]-- ^ all players' bids
--   -> Card         -- ^ trump card
--   -> [Trick]      -- ^ tricks played so far
--   -> Trick        -- ^ cards in the current trick so far
--   -> Card 

-- |Determine if the player is the last to bid.
lastPlayer :: Int -> [Int] -> Bool
lastPlayer p b = (==) ((-) p (length b)) 1

-- |Find the illegal bid for the last player.
illegalBid :: [Card] -> [Int] -> Int
illegalBid c b = (-) (length c) (sum b)

-- |Find the possible bids taking into account the hook rule.
possibleBids :: [Card] -> [Int] -> Bool -> [Int]
possibleBids c b l
    | l == True = [x | x <- [0..(length c)], x /= badBid]
    | otherwise = [x | x <- [0..(length c)]]
    where
        badBid = illegalBid c b

-- | Bid the number of cards you can win based on the trump card and your hand.
makeBid :: BidFunc
makeBid _ c p b = let (x:_) = possibleBids c b (lastPlayer p b) in x

-- type BidFunc
--   = Card    -- ^ trump card
--   -> [Card] -- ^ list of cards in the player's hand
--   -> Int    -- ^ number of players
--   -> [Int]  -- ^ bids so far
--   -> Int    -- ^ the number of tricks the player intends to win