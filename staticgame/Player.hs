module Player (
    playCard,
    makeBid
)
where
{-
The general strategy of the AI is predicated on the idea that it is easier to lose tricks than to win them.
Therefore, it makes a low bid at the start of each trick according to how many trump cards there are in the
hand. 

This is accomplished by folding through a list of possible bids (hook rule taken into account) and 
finding the number closest to the number of trump cards in hand.

The hands themselves are played according to whether the AI needs more tricks to win. The highest card is played
when the player needs to win, the lowest otherwise.
-}

import OhTypes
import OhHell
import Data.List

-- | Adapted from OhHell.hs. Takes a trick and returns Just Suit if there is a leading suit, else Nothing.
leadSuit :: Trick -> Maybe Suit
leadSuit [] = Nothing
leadSuit c = let (Card suit _, _) = last c in Just suit

-- | Filter the cards in hand based on the given Suit and sort them by Rank.
cardsOfSuit :: 
    [Card]          -- the cards in hand
    -> Maybe Suit   -- the Suit to be filtered on
    -> Bool         -- True to get all the cards of the Suit, False to get all the cards not of the given Suit
    -> Bool         -- True to reverse the list else False
    -> [Card]       -- the filtered list of cards
cardsOfSuit cds Nothing _ _ = cds         -- a filter based on a Nothing suit returns an original list of cards.
cardsOfSuit cds (Just st) inv rev 
    | rev == False = sort $ filter isSuit cds
    | otherwise = reverse.sort $ filter isSuit cds
    where isSuit (Card s _)
            | inv == False = if s == st then True else False
            | otherwise = if s == st then False else True

-- | Filter the list of bids to find the bid of a player.
bidOfPlayer :: 
    PlayerId              -- the player id of the player
    -> [(PlayerId, Int)]  -- the bids made for that trick
    -> Int                -- the bid made by the player
bidOfPlayer pId bids = let (x:_) = filter isPlayer bids
    in snd $ x
    where isPlayer bd = if fst bd == pId then True else False

-- | Order the cards by trump Suit, lead Suit, ordered by Rank.
orderCards :: 
    [Card]          -- the list of cards in hand
    -> Suit         -- the current trump Suit
    -> Maybe Suit   -- the lead Suit
    -> Bool         -- True if we are trying to win, False otherwise
    -> [Card]       -- the list of cards in order according to rules of game and of objective
orderCards cds tmpSuit mbLdSuit tryToLose
    -- if there is no lead suit played yet, choose trump as priority if trying to win, non-trump otherwise
    | mbLdSuit == Nothing && tryToLose == False = tmpCards ++ tmpCards'
    | mbLdSuit == Nothing && tryToLose == True = tmpCards' ++ tmpCards
    | (Just tmpSuit) == mbLdSuit = tmpCards ++ tmpLdCards'          -- if the lead suit is the same as trump suit
    -- if the lead suit has to be played, choose trump as priority if trying to win, non-trump otherwise
    | tryToLose == False = ldCards ++ tmpCards ++ tmpLdCards'
    | otherwise = ldCards ++ tmpLdCards' ++ tmpCards          
    where tmpCards = cardsOfSuit cds (Just tmpSuit) False tryToLose       -- all the trump cards
          tmpCards' = cardsOfSuit cds (Just tmpSuit) True tryToLose       -- all the cards not trump
          ldCards = cardsOfSuit cds mbLdSuit False tryToLose              -- all the lead cards
          tmpLdCards' = cardsOfSuit (cardsOfSuit cds (Just tmpSuit) True tryToLose) mbLdSuit True tryToLose  -- all the cards not trump or lead

-- type PlayFunc
--   =  PlayerId     -- ^ this player's Id so they can identify themselves in the bids and tricks
--   -> [Card]       -- ^ the player's cards
--   -> [(PlayerId,Int)]-- ^ all players' bids
--   -> Card         -- ^ trump card
--   -> [Trick]      -- ^ tricks played so far
--   -> Trick        -- ^ cards in the current trick so far
--   -> Card 

{-
    Play a card for the current trick. Based on the number of bids that still need to be won, play the highest or lowest card
    according to the rules of the game.
 -}
playCard :: PlayFunc
playCard pId cds bids (Card tmpSuit _) txs tx
    | wonBds < bidOfPlayer pId bids = head $ orderedCds False      -- if we need to win more tricks, play the highest card
    | wonBds >= bidOfPlayer pId bids = head $ orderedCds True      -- if we need to lose, play the lowest card
        where wonBds = foldr (const(+1)) 0 $ filter (\x -> if x == pId then True else False) $ map (winner tmpSuit) txs
              orderedCds = orderCards cds tmpSuit (leadSuit tx)
playCard _ _ _ (Card _ _) _ _ = undefined                          -- to meet the safePragma

-- | Determine if the player is the last to bid by subtracting player count from number of bids made in trick and equating to 1.
lastPlayer :: 
    Int        -- the number of players in the game
    -> [Int]   -- the list of bids made in the game
    -> Bool    -- whether the player is last to bid
lastPlayer p b = (==) ((-) p (length b)) 1

-- | Find the illegal bid for the last player by subtracting the number of cards in hand from sum of previous bids.
illegalBid :: 
    [Card]      -- the list of cards in hand
    -> [Int]    -- the bids made by the other players
    -> Int      -- the bid that would violate the hook rule
illegalBid cds bds = (-) (length cds) (sum bds)

-- | Filter for the illegal bids.
badBidFilter :: 
    [Card]      -- the list of cards in hand
    -> [Int]    -- the list of the possible bids
    -> Int      -- the current bid being checked
    -> Bool     -- whether the filter condition has been met
badBidFilter cds bds bd = if bd /= badBid then True else False
    where badBid = illegalBid cds bds

-- | Find the possible bids taking into account the hook rule.
possibleBids :: 
    [Card]      -- the list of cards in hand
    -> [Int]    -- the list of bids by all other players
    -> Bool     -- whether the player is last to play
    -> [Int]    -- a list of the possible bids available to the player
possibleBids cds bds lstPlyr
    | lstPlyr == True = filter (badBidFilter cds bds)  bidList
    | otherwise = bidList
    where
        bidList = [0..(length cds)]

-- | Count the number of trumps in the hand.
countTrumps :: 
    [Card]  -- the list of cards to be counted over
    -> Suit -- the trump suit
    -> Int  -- an integer giving the number of trump cards in the hand
countTrumps cds tmp = length tmpCards
    where tmpCards = cardsOfSuit cds (Just tmp) False False

-- | Find the closest number to the number of trump suits in the list of possible bids.
closestPossible :: 
    [Int]   -- a list of the possible bids available to the player
    -> Int  -- the count of the number of trumps in hand
    -> Int  -- the closest possible bid to the number of trumps in hand playable
closestPossible possible count = foldr (\i a -> if (abs $ (-) count i) <= (abs $ (-) count a) then i else a) 1000 possible

-- type BidFunc
--   = Card    -- ^ trump card
--   -> [Card] -- ^ list of cards in the player's hand
--   -> Int    -- ^ number of players
--   -> [Int]  -- ^ bids so far
--   -> Int    -- ^ the number of tricks the player intends to win

{-
    Makes a bid based on the closest possible number available to the player taking into account the hook rule.
    `tmpCount` is the number of trump cards in hand and `possible` is the list of bids available after the hook
    rule.
 -}
makeBid :: BidFunc
makeBid (Card tmpSuit _) cds noPlyrs bids = closestPossible possible tmpCount
    where tmpCount = countTrumps cds tmpSuit
          possible = possibleBids cds bids (lastPlayer noPlyrs bids)

{-
    Test input
    a = [(Card Spade Two), (Card Diamond Two), (Card Heart Ace), (Card Club Two), (Card Heart Two)]
    bids = [("1", 10), ("2", 20), ("3", 30)]
    a = [Card Club Two,Card Heart Four,Card Heart Ace,Card Heart Three,Card Diamond Six,Card Spade Two,Card Heart Six]
    trick = [(Card Heart Two, "1"), (Card Diamond Two, "2") ,(Card Heart Ace, "3")]
 -}