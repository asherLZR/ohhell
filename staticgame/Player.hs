module Player (
    playCard,
    makeBid
)
where
{-
Write a report describing your design and strategy here.

Perform a filter operation on the cards in hand to see if a suit can be followed.

How to deal with no cards returned in base case?
-}

import OhTypes
import OhHell

-- Given a Trick, returns the suit of the last card played.
lastSuitInTrick :: Trick -> Suit
lastSuitInTrick [] = Spade
lastSuitInTrick (((Card suit _), _):[]) = suit
lastSuitInTrick (_:xs) = lastSuitInTrick xs

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard _ [] _ _ _ _ = Card Spade Two      -- but there can never be no cards in hand?
playCard _ (card:_) _ _ _ curTrick = card

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