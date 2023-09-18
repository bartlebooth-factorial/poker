module Poker where

-- Implementation of hand-ordering in Poker

import Data.List

-- Rank: just a number
-- 11,12,13,[14 || 1] will mean j,q,k,a respectively
type Rank = Integer

-- Suit: one of 4 named values
data Suit =
  Club | Diamond | Heart | Spade
  deriving (Show, Eq, Ord)

-- Card: ordered pair of a Rank and a Suit
type Card =
  (Rank, Suit)

-- Hand: list of Cards (by convention, 5 cards)
type Hand = [Card]

-- Score: my term for the different value categories a hand can fall into
data Score =
    HighCard | Pair | TwoPair | ThreeOfAKind
    | Straight | Flush | FullHouse | FourOfAKind
    | StraightFlush | RoyalFlush
  deriving (Show, Eq, Ord)

-- + SCORE CHECKING

-- | When two hands fall into the same score category, the winner is
-- determined by the "kicker" of the hand. This is usually the
-- high-card of the hand (in the case of a straight or a flush) but it
-- can also be the set of ranks for which an n-group exists (in the
-- case of (two-)pair/threeOAK/fourOAK/fullHouse): for instance, 5s
-- full beats 4s full, and 5s full of 6s beats 5s full of 4s.
data Kicker =
  KickCard Card | KickRanks [Rank]
  deriving (Show)

-- | The patterns for Eq and Ord don't need to be exhaustive,
-- as we will only ever compare Kickers of the same subtype.
instance Eq Kicker where
  (==) (KickCard cx) (KickCard cy) = cx == cy
  (==) (KickRanks rsx) (KickRanks rsy) = (sort rsx) == (sort rsy)

instance Ord Kicker where
  (<=) (KickCard cx) (KickCard cy) = cx <= cy
  (<=) (KickRanks rsx) (KickRanks rsy) = (sort rsx) <= (sort rsy)

-- | Provide a way to apply functions that take a card to Kickers
-- that hold a card
kickerAppC :: (Card -> a) -> Kicker -> a
kickerAppC f (KickCard c) = f c

type ScoreChecker = Hand -> (Bool, Kicker)

-- | Builtin list sort compares =fst=s first and =snd=s if =fst=s tie;
-- conveniently, this is the desired behavior.
sortHand :: Hand -> Hand
sortHand hand = sort hand

highCard :: Hand -> Card
highCard hand = last (sortHand hand)

isHighCard :: ScoreChecker
isHighCard hand =
  (True, KickCard (highCard hand))

rankOccur :: Hand -> Rank -> Integer
rankOccur hand rank =
  sum $ map (\card ->
               if (fst card) == rank
               then 1
               else 0) hand

ns :: Hand -> Integer -> [Rank]
ns hand n =
  nub $ traverseCount hand where
  traverseCount [] = []
  traverseCount (card:cards) =
    let rank = fst card
    in if rankOccur hand rank == n
       then rank : traverseCount cards
       else traverseCount cards

pairs :: Hand -> [Rank]
trips :: Hand -> [Rank]
quads :: Hand -> [Rank]

pairs hand = ns hand 2
trips hand = ns hand 3
quads hand = ns hand 4

isPair :: ScoreChecker
isTwoPair :: ScoreChecker
isThreeOfAKind :: ScoreChecker
isFullHouse :: ScoreChecker
isFourOfAKind :: ScoreChecker

isPair hand =
  let ps = pairs hand
  in (length ps == 1
     , KickRanks ps)

isTwoPair hand =
  let ps = pairs hand
  in (length ps == 2
     , KickRanks ps)

isThreeOfAKind hand =
  let ts = trips hand
  in (length ts == 1
     , KickRanks ts)

isFullHouse hand =
  let ps = pairs hand
      ts = trips hand
  in (length ps == 1 && length ts == 1
     , KickRanks (ts ++ ps))

isFourOfAKind hand =
  let qs = quads hand
  in (length qs == 1
     , KickRanks qs)

isStraight :: ScoreChecker
isStraight hand =
  let sortedHand = sortHand hand
      handHighCard = highCard hand
      highRank = fst handHighCard
      lowball = (0, Spade) -- under-ranks all valid cards
      topper = (highRank + 1, Spade) -- one higher than highest card in hand
  in (not $ (foldr (\c1 c2 ->
                      if (fst c1) + 1 == (fst c2)
                      then c1
                      else lowball) topper sortedHand) == lowball
     , KickCard handHighCard)

isFlush :: ScoreChecker
isFlush hand =
  let handHighCard = highCard hand
      suit = snd handHighCard
  in (filter (\card ->
                (snd card) == suit) hand
      == hand
     , KickCard handHighCard)

isStraightFlush :: ScoreChecker
isRoyalFlush :: ScoreChecker

isStraightFlush hand =
  let (straightCheck, kickerCard) = isStraight hand
      flushCheck = fst (isFlush hand)
  in (straightCheck && flushCheck
     , kickerCard)

isRoyalFlush hand =
  let (straightFlushCheck, kickerCard) = isStraightFlush hand
  in (straightFlushCheck && kickerAppC (\card -> card > (13, Spade)) kickerCard
     , kickerCard)

-- + HAND COMPARISON

-- | When a hand is compared to another, it can either lose, tie, or
-- win.
data Result =
  Loss | Tie | Win
  deriving (Show, Eq, Ord)

boolToRes :: Bool -> Result
boolToRes False = Loss
boolToRes True = Win

-- | List of ScoreCheckers, highest->lowest value.
scoreBoard =
  [isRoyalFlush, isStraightFlush, isFourOfAKind, isFullHouse,
   isFlush, isStraight, isThreeOfAKind, isTwoPair,
   isPair, isHighCard]

-- | The big conclusion. isHighCard always returns (True, _), so
-- predicateRace will always terminate and will never run off the end
-- of scoreBoard.
handCmp :: Hand -> Hand -> Result
handCmp hx hy =
  predicateRace scoreBoard
  where
    predicateRace (check:checks) =
      let (checkX, kickerX) = check hx
          (checkY, kickerY) = check hy
      in if checkX && checkY
         then if kickerX == kickerY
              then Tie
              else boolToRes (kickerX > kickerY)
        else if checkX || checkY
             then boolToRes checkX
             else predicateRace checks

