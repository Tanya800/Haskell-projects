module Lists where

second:: [a]->a
second lst = head (tail lst)

-- сопоставление с образцом стандартные функции head и tail
head' ((:) x _)=x
tail' ((:) _ xs)= xs 

second':: [a]->a
second' (_:xs) = head xs

second'':: [a]->a
second'' (_:x:_) = x

sndHead' (((,)_ x):_)= x

--  количество элементов в списке
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1+ length' xs

--  список целых нечетных чисел
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly lst@(x:xs) 
					| (x `mod` 2) == 0 = oddsOnly xs
					| otherwise = x : oddsOnly xs

--  сумма элементов списка
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--  произведение элементов списка
product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * sum' xs

--  максимум и минимум элеметов в списке

minElem [] = 0 
minElem lst@(x:_) = minn lst x

minn [] n = n
minn lst@(x:xs) n | n < x = minn xs n
				  | otherwise = minn xs x
 
maxElem [] = 0
maxElem lst@(x:_) = max' lst x
max' [] n = n
max' (x:xs) n | n> x = max' xs n
			  | otherwise = max' xs x

reverse':: [a] -> [a]
reverse' l = rev l [] 
		where
			rev [] a = a
			rev (x:xs) a = rev xs (x:a)

--  проверка является ли список полиндромом 
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome lst = lst == (reverse' lst) 