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

-- From OhHell.hs
leadSuit :: Trick -> Suit
leadSuit c = let (Card suit _, _) = last c in suit

-- Filter the cards in hand based on the given Suit.
cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cds st = let isSuit (Card s _) = if s == st then True else False
    in filter isSuit cds

-- Filter the list of bids to find the bid of a player.
bidOfPlayer :: PlayerId -> [(PlayerId, Int)] -> Int
bidOfPlayer pId bids = let (x:_) = filter isPlayer bids
    in snd $ x
    where isPlayer bd = if fst bd == pId then True else False

-- Sort the cards by Rank.
-- rankSort :: [Card] -> [Card]
-- rankSort = 

-- trumpSuit
-- leadSuit
-- lowestCard

-- cardsOfSuit cds trumpSuit
-- cardsOfSuit cds leadSuit
-- lowestCard/highestCard cds (low if trying to win, high if trying to lose)

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard _ _ _ _ _ t = Card (leadSuit t) Two
playCard pId cds bids tmp txs tx = 
    where lSt = leadSuit t

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