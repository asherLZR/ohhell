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
 a = [Card Club Two,Card Heart Four,Card Heart Ace,Card Heart Three,Card Diamond Six,Card Spade Two,Card Heart Six]
-}

import OhTypes
import OhHell
import Data.List

-- | Adapted from OhHell.hs
leadSuit :: Trick -> Maybe Suit
leadSuit [] = Nothing
leadSuit c = let (Card suit _, _) = last c in Just suit

-- | Filter the cards in hand based on the given Suit and sort them by Rank.
cardsOfSuit :: [Card] -> Maybe Suit -> Bool -> Bool -> [Card]
cardsOfSuit cds Nothing _ _ = cds         -- | A filter based on a Nothing suit returns an original list of cards.
cardsOfSuit cds (Just st) inv rev 
    | rev == False = sort $ filter isSuit cds
    | otherwise = reverse.sort $ filter isSuit cds
    where isSuit (Card s _)
            | inv == False = if s == st then True else False
            | otherwise = if s == st then False else True

-- | Filter the list of bids to find the bid of a player.
bidOfPlayer :: PlayerId -> [(PlayerId, Int)] -> Int
bidOfPlayer pId bids = let (x:_) = filter isPlayer bids
    in snd $ x
    where isPlayer bd = if fst bd == pId then True else False

-- | Order the cards by trump Suit, lead Suit, ordered by Rank
orderCards :: [Card] -> Suit -> Maybe Suit -> Bool -> [Card]
orderCards cds tmpSuit mbLdSuit rev
    | mbLdSuit == Nothing = tmpCards ++ tmpCards'
    | (Just tmpSuit) == mbLdSuit = tmpCards ++ tmpLdCards'
    | otherwise = ldCards ++ tmpCards ++ tmpLdCards'
    where tmpCards = cardsOfSuit cds (Just tmpSuit) False rev
          tmpCards' = cardsOfSuit cds (Just tmpSuit) True rev
          ldCards = cardsOfSuit cds mbLdSuit False rev
          tmpLdCards' = cardsOfSuit (cardsOfSuit cds (Just tmpSuit) True rev) mbLdSuit True rev

-- | Play a card for the current trick. If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard pId cds bids (Card tmpSuit _) txs tx
    | wonBds < bidOfPlayer pId bids = head $ orderedCds False      -- if we need to win more tricks, play the highest card
    | wonBds >= bidOfPlayer pId bids = head $ orderedCds True      -- if we need to lose, play the lowest card
        where wonBds = foldr (const(+1)) 0 $ filter (\x -> if x == pId then True else False) $ map (winner tmpSuit) txs
              orderedCds = orderCards cds tmpSuit (leadSuit tx)
playCard _ _ _ (Card _ _) _ _ = undefined

-- | Determine if the player is the last to bid by subtracting player count from number of bids made in trick.
lastPlayer :: Int -> [Int] -> Bool
lastPlayer p b = (==) ((-) p (length b)) 1

-- | Find the illegal bid for the last player by subtracting the number of cards in hand from sum of previous bids.
illegalBid :: [Card] -> [Int] -> Int
illegalBid cds bds = (-) (length cds) (sum bds)

-- | Find the possible bids taking into account the hook rule.
possibleBids :: [Card] -> [Int] -> Bool -> [Int]
possibleBids cds bds lstPlyr
    | lstPlyr == True = filter (\i -> if i /= badBid then True else False) bidList
    | otherwise = bidList
    where
        bidList = [x | x <- [0..(length cds)]]
        badBid = illegalBid cds bds

-- | Count the number of trumps in the hand.
countTrumps :: [Card] -> Suit -> Int
countTrumps cds tmp = length tmpCards
    where tmpCards = cardsOfSuit cds (Just tmp) False False

-- | Find the closest number to the number of trump suits in the list of possible bids.
closestPossible :: [Int] -> Int -> Int
closestPossible possible count = foldr (\i a -> if (abs $ (-) count i) <= (abs $ (-) count a) then i else a) 1000 possible

-- | Bid the number of cards you can win based on the trump card and your hand.
makeBid :: BidFunc
makeBid (Card tmpSuit _) cds noPlyrs bids = closestPossible possible tmpCount
    where tmpCount = countTrumps cds tmpSuit
          possible = possibleBids cds bids (lastPlayer noPlyrs bids)

-- type PlayFunc
--   =  PlayerId     -- ^ this player's Id so they can identify themselves in the bids and tricks
--   -> [Card]       -- ^ the player's cards
--   -> [(PlayerId,Int)]-- ^ all players' bids
--   -> Card         -- ^ trump card
--   -> [Trick]      -- ^ tricks played so far
--   -> Trick        -- ^ cards in the current trick so far
--   -> Card 

-- type BidFunc
--   = Card    -- ^ trump card
--   -> [Card] -- ^ list of cards in the player's hand
--   -> Int    -- ^ number of players
--   -> [Int]  -- ^ bids so far
--   -> Int    -- ^ the number of tricks the player intends to win