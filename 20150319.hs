double :: [Int] -> [Int]
double list
    | list == [] = []
    | otherwise = (head list * 2) : double(tail list)
	
member :: [Int] -> Int -> Bool
member list element
    | list == [] = False
    | head list == element = True
    | otherwise = member (tail list) (element)

digits :: String -> String
digits list
    | list == [] = []
    | (head list >= '0') && (head list <= '9') = head list : digits(tail list)
    | otherwise = digits(tail list)
	
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs list1 list2
	| (list1 == []) && (list2 == []) = []
	| list1 == [] = head list2 : sumPairs (list1) (tail list2)
    | list2 == [] = head list1 : sumPairs (tail list1) (list2)
    | otherwise = (head list1 + head list2) : sumPairs (tail list1) (tail list2)