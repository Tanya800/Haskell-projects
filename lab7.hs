sum' = zipWith (+) 
f list = foldl (\ x acc -> x* 10   + acc ) 0 list
--число из списка сумм двух списков
sumList' l1 l2 = foldl (\ x acc -> x * 10 + acc) 0 $ zipWith (+) l1 l2

-- сумма двух списков с разной длинной
suml' l1 l2 = reverse $ sumL' ( reverse l1) (reverse l2)

sumL' [] [] = []
sumL' [] l2 = l2
sumL' l1 [] = l1
sumL' (l:ls) (p:ps) = l+p : sumL' ls ps