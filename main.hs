


post [] = []
post lst@(_:ls) = [lst] ++ post(ls)

init' :: [a] -> [a]
init' [x] =[]
init' (x:xs) = x : init xs

pref :: [a] -> [[a]]
pref [] = []
pref lst = [lst] ++ pref lst'
	where 
		lst' = init' lst 

segm [] = [] 
segm lst =  (++) lst1 lst2
	where 
		lst1= pref lst
		lst2 = post lst

permutations :: [a] -> [[a]]
permutations xs = doPerm xs []
  where
    doPerm [] _ = [[]]
    doPerm [y] ys = (y:) <$> doPerm ys []
    doPerm (y : ys) zs = doPerm [y] (ys ++ zs) ++ doPerm ys (y : zs)