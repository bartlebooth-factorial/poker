module Test where

import Poker

-- not straight, flush
hand1 :: Hand
hand1 = [(3, Diamond), (4, Diamond), (2, Diamond), (11, Diamond), (12, Diamond)]

-- (not straight but sorted), not flush
hand2 = [(3, Heart), (4, Diamond), (8, Diamond), (11, Diamond), (14, Diamond)]

-- straight, flush
hand3 = [(3, Diamond), (4, Diamond), (5, Diamond), (6, Diamond), (7, Diamond)]

-- royal flush
hand4 = [(10, Spade), (11, Spade), (12, Spade), (13, Spade), (14, Spade)]

-- straight (out of order to check sorting)
hand5 = [(5, Club), (4, Club), (6, Spade), (3, Heart), (7, Diamond)]

-- 5s full of 3s
hand6 = [(5, Club), (5, Spade), (5, Heart), (3, Heart), (3, Diamond)]

-- two pair, 6s and 7s
hand7 = [(7, Club), (7, Spade), (6, Heart), (10, Heart), (6, Diamond)]

-- Sep 17 3:30pm: Testing handCmp in repl produces all correct results
