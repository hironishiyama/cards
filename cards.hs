import Data.Char
import Data.List
import System.IO
import System.Random
import Control.Monad

data Suit = Spade | Heart | Diamond | Club deriving(Eq, Ord, Enum, Read, Show, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
     deriving (Eq, Ord, Enum, Read, Show, Bounded)
data Card = Joker | Card {rank :: Rank, suit :: Suit} deriving (Read, Eq)
data Color = NoColor | Red | Black deriving (Eq, Show)
data Hand = Hand Card Card Card Card Card
data HandKind = NoPair | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse |
                FourOfAKind | StraightFlush | RoyalStraightFlush | FiveOfAKind deriving (Eq, Ord)

instance Ord Card where
    compare c1 c2 | c1 == Joker = LT
                  | c2 == Joker = GT
                  | (rank c1 == (rank c2)) = compare (suit c1) (suit c2)
                  | otherwise = compare (rank c1) (rank c2)

instance Enum Card where
    toEnum n
      | n == 0    = Joker
      | otherwise = Card (toEnum ((n + 1) `div` 4)) (toEnum ((n + 1) `mod` 4))
    fromEnum c
      | c == Joker = 0
      | otherwise  = 4 * (fromEnum (rank c)) + (fromEnum (suit c)) + 1

instance Show Card where
    show c
      | c == Joker = (escSeq $ color c) ++ "Joker" ++ restoreColor
      | otherwise  = (escSeq $ color c) ++ show (rank c) ++ " of " ++ show (suit c) ++ restoreColor

instance Show Hand where
    show (Hand c1 c2 c3 c4 c5) = show c1 ++ "\n" ++ show c2 ++ "\n" ++ show c3 ++ "\n" ++ show c4 ++ "\n" ++ show c5

instance Show HandKind where
    show hk
      | hk == OnePair = "One Pair"
      | hk == TwoPair = "Two Pair"
      | hk == ThreeOfAKind = "Three Of A Kind"
      | hk == Straight = "Straight"
      | hk == Flush = "Flush"
      | hk == FullHouse = "Full House"
      | hk == FourOfAKind = "Four Of A Kind"
      | hk == StraightFlush = "Straight Flush"
      | hk == RoyalStraightFlush = "Royal Straight Flush"
      | hk == FiveOfAKind = "Five Of A Kind"
      | otherwise = "No Pair"

color :: Card -> Color
color c
  | c == Joker = NoColor
  | otherwise  = if (suit c) == Spade || (suit c) == Club then Black else Red

sameColor :: Card -> Card -> Bool
sameColor a b = color a == color b

alternateColors :: Card -> Card -> Bool
alternateColors a b = color a /= color b

follows :: Card -> Card -> Bool
follows (Card Ace _) _ = False
follows (Card v1 _) (Card v2 _) = succ v1 == v2

skip :: Card -> Card -> Bool
skip (Card Ace _) _ = False
skip (Card King _) _ = False
skip (Card v1 _) (Card v2 _) = succ (succ v1) == v2

escSeq :: Color -> String
escSeq c
  | c == Black = "\x1b[47m\x1b[30m"
  | c == Red   = "\x1b[47m\x1b[31m"
  | otherwise  = "\x1b[47m\x1b[33m"

restoreColor :: String
restoreColor = "\x1b[40m\x1b[37m"

type Deck = [Card]
type Shuffle = (Deck -> IO Deck)
type ShuffleSequence = [Shuffle]

deck :: Deck
deck = Joker : [Card r s | r <- [Two .. Ace], s <- [Spade .. Club]]

noJoker :: Deck -> Deck
noJoker d = filter (/= Joker) d

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)

perfect_shuffle :: Shuffle
perfect_shuffle d = do
    return (interleave first second) where
    first = take half d
    second = drop half d
    half = (length d) `div` 2

randomD :: IO Double
randomD = getStdRandom $ randomR(0.0, 1.0) :: IO Double

interleaveR :: [a] -> [a] -> IO [a]
interleaveR xs [] = return xs
interleaveR [] ys = return ys
interleaveR xs ys = do
    rand1 <- randomD
    rand2 <- randomD
    let n = if rand1 > 0.95 then 3 else if rand1 > 0.8 then 2 else 1
        m = if rand2 > 0.95 then 3 else if rand2 > 0.8 then 2 else 1
    r <- interleaveR (drop n xs) (drop m ys)
    return ((take n xs) ++ (take m ys) ++ r)  

raffle_shuffle :: Shuffle
raffle_shuffle d = do
    rand <- randomD
    if rand > 0.5 then do
        r <- interleaveR first second
        return r
    else do
        r <- interleaveR second first
        return r
    where
        first = take half d
        second = drop half d
        half = (length d) `div` 2

hindu_shuffle :: Shuffle
hindu_shuffle d = do
    rand1 <- getStdRandom $ randomR (0.2, 0.5) :: IO Double
    rand2 <- getStdRandom $ randomR (0.5, 0.7) :: IO Double
    let start = floor $ rand1 * (fromIntegral $ length d)
        end = min (start + (floor $ rand2 * (fromIntegral $ length d))) (length d)
    -- putStrLn $ (show start) ++ " " ++ (show end)
    return ((drop start $ take end d) ++ (take start d) ++ (drop end d))

makeShuffleSequence :: String -> ShuffleSequence
makeShuffleSequence "" = []
makeShuffleSequence (x:xs)
  | toLower x == 'p' = perfect_shuffle : makeShuffleSequence xs
  | toLower x == 'r' = raffle_shuffle : makeShuffleSequence xs
  | toLower x == 'h' = hindu_shuffle : makeShuffleSequence xs
  | otherwise = error "wrong sequence string" 

doShuffle :: ShuffleSequence -> Shuffle
doShuffle [] d = return d
doShuffle (s:ss) d = do
    d <- s d
    d <- doShuffle ss d
    return d

deal :: Deck -> IO (Hand, Deck)
deal d = do return ((Hand (sd!!0) (sd!!1) (sd!!2) (sd!!3) (sd!!4)), drop 5 d) where
    sd = sort $ take 5 d

draw :: String -> Hand -> Deck -> IO (Hand, Deck)
draw s h@(Hand c1 c2 c3 c4 c5) dd = do
    return (Hand (sd!!0) (sd!!1) (sd!!2) (sd!!3) (sd!!4), drop n dd) where
    sd = sort $ change s [c1, c2, c3, c4, c5] dd
    n = length $ filter (=='1') s

change :: String -> [Card] -> Deck -> [Card]
change _ _ [] = error "enpty deck."
change [] c _ = c
change _ [] _ = []
change xxs@(x:xs) ccs@(c:cs) (d:ds)
  | x == '1'  = d : (change xs cs ds) 
  | otherwise = c : (change xs cs (d:ds))

hasJoker :: Hand -> Bool
hasJoker (Hand c1 _ _ _ _) = if c1 == Joker then True else False

pairList :: Hand -> [(Rank, Int)]
pairList h@(Hand c1 c2 c3 c4 c5)
  | hasJoker h = filter (\a -> snd a > 1) $ map (\l@(x:xs) -> (x,length l)) . group $ [rank c2, rank c3, rank c4, rank c5]
  | otherwise  = filter (\a -> snd a > 1) $ map (\l@(x:xs) -> (x,length l)) . group $ [rank c1, rank c2, rank c3, rank c4, rank c5]

onePair :: Hand -> Bool
onePair h 
  | hasJoker h = True
  | otherwise  = if length (pairList h) > 0 then True else False

twoPair :: Hand -> Bool
twoPair h
  | hasJoker h = if length (pairList h) > 0 then True else False
  | otherwise  = if length (pairList h) > 1 then True else False

threeOfAKind :: Hand -> Bool
threeOfAKind h
  | hasJoker h = if length (pairList h) > 0 then True else False
  | otherwise  = if length (pairList h) > 0 && maximum (map snd $ pairList h) > 2 then True else False

fullHouse :: Hand -> Bool
fullHouse h
  | hasJoker h = if length (pairList h) == 2 || (length (pairList h) > 0 && maximum (map snd $ pairList h) == 3) then True else False
  | otherwise  = if threeOfAKind h && twoPair h then True else False

fourOfAKind :: Hand -> Bool
fourOfAKind h
  | hasJoker h = if length (pairList h) > 0 && maximum (map snd $ pairList h) >= 3 then True else False
  | otherwise  = if length (pairList h) > 0 && maximum (map snd $ pairList h) == 4 then True else False

fiveOfAKind :: Hand -> Bool
fiveOfAKind h = if hasJoker h && length (pairList h) > 0 && maximum (map snd $ pairList h) == 4 then True else False

flush :: Hand -> Bool
flush h@(Hand c1 c2 c3 c4 c5)
  | hasJoker h = suit c2 == suit c3 && suit c3 == suit c4 && suit c4 == suit c5
  | otherwise  = suit c1 == suit c2 && suit c2 == suit c3 && suit c3 == suit c4 && suit c4 == suit c5

straight :: Hand -> Bool
straight h@(Hand c1 c2 c3 c4 c5)
  | hasJoker h = (follows c2 c3 && follows c3 c4 && follows c4 c5) ||
                 (skip c2 c3 && follows c3 c4 && follows c4 c5) ||
                 (follows c2 c3 && follows c3 c4 && skip c4 c5) ||
                 (follows c2 c3 && skip c3 c4 && follows c4 c5) || (aceStraight h)
  | otherwise  = (follows c1 c2 && follows c2 c3 && follows c3 c4 && follows c4 c5) || (aceStraight h)

aceStraight :: Hand -> Bool
aceStraight h@(Hand c1 c2 c3 c4 c5)
  | hasJoker h = (rank c5 == Ace) && 
                 (elem (rank c2, rank c3, rank c4)
                      [(Two, Three, Four), (Two, Three, Five), (Two, Four, Five), (Three, Four, Five)])
  | otherwise  = (rank c1, rank c2, rank c3, rank c4, rank c5) == (Two, Three, Four, Five, Ace)

straightFlush :: Hand -> Bool
straightFlush h = straight h && flush h

royalStraightFlush :: Hand -> Bool
royalStraightFlush h@(Hand c1 c2 c3 c4 c5) = (not. hasJoker $ h) && flush h && straight h && rank c5 == Ace
                                             && (not. aceStraight $ h)

hand :: Hand -> HandKind
hand h
  | fiveOfAKind h = FiveOfAKind
  | royalStraightFlush h = RoyalStraightFlush
  | straightFlush h = StraightFlush
  | fourOfAKind h = FourOfAKind
  | fullHouse h   = FullHouse
  | flush h       = Flush
  | straight h    = Straight
  | threeOfAKind h = ThreeOfAKind
  | twoPair h     = TwoPair
  | onePair h     = OnePair
  | otherwise     = NoPair

extremelyCleverArtificialIntelligence :: Hand -> IO String
extremelyCleverArtificialIntelligence _ = replicateM 5 (getStdRandom $ randomR ('0', '1') :: IO Char)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

showHand :: Hand -> IO ()
showHand h = do
  print h
  putStrLn $ "---------\n" ++ show (hand h)
 
main :: IO ()
main = do
  let ss = makeShuffleSequence "hhhhhhrhhhhhhrhhhhhhrhh"
  deck <- doShuffle ss deck
  (hYou, deck) <- deal deck
  (hCpu, deck) <- deal deck

  putStrLn "\nyour turn :\n"
  showHand hYou
  c <- prompt "select cards to draw : "
  (hYou, deck) <- draw c hYou deck

  putStr "\ncomputer's turn : "
  drawStr <- extremelyCleverArtificialIntelligence hCpu
  (hCpu, deck) <- draw drawStr hCpu deck
  putStrLn $ "draw " ++ show (length $ filter (=='1') drawStr) ++ " cards"

  putStrLn "\nyour turn :\n"
  showHand hYou
  c <- prompt "select cards to draw : "
  (hYou, deck) <- draw c hYou deck

  putStr "\ncomputer's turn : "
  drawStr <- extremelyCleverArtificialIntelligence hCpu
  (hCpu, deck) <- draw drawStr hCpu deck
  putStrLn $ "draw " ++ show (length $ filter (=='1') drawStr) ++ " cards"

  putStrLn "\nyour hand : "
  showHand hYou

  putStrLn "\ncomputer's hand : "
  showHand hCpu
  
  if hand hYou > hand hCpu then putStrLn "you win"
  else if hand hYou == hand hCpu then putStrLn "draw game"
  else putStrLn "computer wins" 

  putStrLn "\nhit return for next deal"
  c <- getLine
  main
