import Data.List

main = do
          wholeFile <- readFile "p054_poker.txt"
          print $ process wholeFile

process wholeFile = length player1Wins
        where lists = map listsOfCards $ lines wholeFile
              hands = map handsFromListsOfCards lists
              player1Wins = winningHandsPlayer1 hands

listsOfCards line = toListsOfCards $ words line

handsFromListsOfCards (cards1, cards2) = (toPokerHand cards1, toPokerHand cards2)

winningHandsPlayer1 hands = filter (\(h1, h2) -> h1>h2) hands

toListsOfCards ws = (\(c1,c2) -> (reverse $ sort c1, reverse $ sort c2)) $ splitAt 5 $ map toCard ws

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Eq, Ord)

data Value = Val2 | Val3 | Val4 | Val5 | Val6 | Val7 | Val8 | Val9 | Val10 | ValJ | ValQ | ValK | ValA deriving (Show, Eq, Ord)

data Card = Card { value :: Value, suit :: Suit } deriving (Show, Eq, Ord)

toCard :: String -> Card
toCard (s0:s1:ss) = Card {value=theValue, suit=theSuit}
        where theValue = toValue s0
              theSuit = toSuit s1

toValue :: Char -> Value
toValue '2' = Val2
toValue '3' = Val3
toValue '4' = Val4
toValue '5' = Val5
toValue '6' = Val6
toValue '7' = Val7
toValue '8' = Val8
toValue '9' = Val9
toValue 'T' = Val10
toValue 'J' = ValJ
toValue 'Q' = ValQ
toValue 'K' = ValK
toValue 'A' = ValA

toSuit :: Char -> Suit
toSuit 'C' = Clubs
toSuit 'S' = Spades
toSuit 'H' = Hearts
toSuit 'D' = Diamonds

valueToInt :: Value -> Int
valueToInt Val2 = 2
valueToInt Val3 = 3
valueToInt Val4 = 4
valueToInt Val5 = 5
valueToInt Val6 = 6
valueToInt Val7 = 7
valueToInt Val8 = 8
valueToInt Val9 = 9
valueToInt Val10 = 10
valueToInt ValJ = 11
valueToInt ValQ = 12
valueToInt ValK = 13
valueToInt ValA = 14

data PokerHand a b = HighCard a b |
              OnePair a b |
              TwoPairs a b |
              ThreeOfAKind a b |
              Straight a |
              Flush a b |
              FullHouse a b |
              FourOfAKind a b |
              StraightFlush a |
              RoyalFlush
              deriving (Show, Eq, Ord)

toPokerHand :: [Card] -> PokerHand Value [Value]
toPokerHand cards@(c1:c2:c3:c4:c5:cs)
        | isRoyalFlush cards = RoyalFlush
        | isStraightFlush cards = StraightFlush (value c1)
        | not (null foak2) = FourOfAKind foak1 foak2
        | not (null full2) = FullHouse full1 full2
        | isFlush cards = Flush (value c1) [value c2, value c3, value c4, value c5]
        | isStraight cards = Straight (value c1)
        | not (null thre2) = ThreeOfAKind thre1 thre2
        | not (null twop2) = TwoPairs twop1 twop2
        | not (null onep2) = OnePair onep1 onep2
        | otherwise = HighCard (value c1) [value c2, value c3, value c4, value c5]
        where (foak1, foak2) = matchFourOfAKind cards
              (full1, full2) = matchFullHouse cards
              (thre1, thre2) = matchThreeOfAKind cards
              (twop1, twop2) = matchTwoPairs cards
              (onep1, onep2) = matchOnePair cards

isRoyalFlush cards@(c1:c2:c3:c4:c5:cs) = isStraightFlush cards
                                         && value c5 == Val10

isStraightFlush cards@(c1:c2:c3:c4:c5:cs) = isFlush cards && isStraight cards

matchFourOfAKind cards@(c1:c2:c3:c4:c5:cs)
        | allSameValue [c1, c2, c3, c4] = (value c1, [value c5])
        | allSameValue [c2, c3, c4, c5] = (value c2, [value c1])
        | otherwise = (Val2, [])

matchFullHouse cards@(c1:c2:c3:c4:c5:cs)
        | allSameValue [c1, c2] && allSameValue [c3, c4, c5] = (value c3, [value c1])
        | allSameValue [c1, c2, c3] && allSameValue [c4, c5] = (value c1, [value c4])
        | otherwise = (Val2, [])

isFlush cards@(c1:c2:c3:c4:c5:cs) = allSameSuit cards

isStraight cards@(c1:c2:c3:c4:c5:cs) = (allDifferentValues cards)
                                       && (valueToInt $ value c1) - (valueToInt $ value c5) == 4

matchThreeOfAKind cards@(c1:c2:c3:c4:c5:cs)
        | allSameValue [c1, c2, c3] = (value c1, [value c4, value c5])
        | allSameValue [c2, c3, c4] = (value c2, [value c1, value c5])
        | allSameValue [c3, c4, c5] = (value c3, [value c1, value c2])
        | otherwise = (Val2, [])

matchTwoPairs cards@(c1:c2:c3:c4:c5:cs)
        | allSameValue [c1, c2] && allSameValue [c3, c4] = (max (value c1) (value c3), [value c5])
        | allSameValue [c1, c2] && allSameValue [c4, c5] = (max (value c1) (value c4), [value c3])
        | allSameValue [c2, c3] && allSameValue [c4, c5] = (max (value c2) (value c4), [value c1])
        | otherwise = (Val2, [])

matchOnePair cards@(c1:c2:c3:c4:c5:cs)
        | allSameValue [c1, c2] = (value c1, [value c3, value c4, value c5])
        | allSameValue [c2, c3] = (value c2, [value c1, value c4, value c5])
        | allSameValue [c3, c4] = (value c3, [value c1, value c2, value c5])
        | allSameValue [c4, c5] = (value c4, [value c1, value c2, value c3])
        | otherwise = (Val2, [])

allSameSuit :: [Card] -> Bool
allSameSuit [c] = True
allSameSuit (c1:c2:cs) = (suit c1) == (suit c2) && allSameSuit (c2:cs)

allDifferentValues :: [Card] -> Bool
allDifferentValues [c] = True
allDifferentValues (c1:c2:cs) = (value c1) /= (value c2) && allDifferentValues (c2:cs)

allSameValue :: [Card] -> Bool
allSameValue [c] = True
allSameValue (c1:c2:cs) = (value c1) == (value c2) && allSameValue (c2:cs)

