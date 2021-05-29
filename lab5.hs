

post :: [a] -> [[a]]
post [] = [[]]
post lst@(_:ls) = [lst] ++ post(ls)

init' :: [a] -> [a]
init' [x] =[]
init' (x:xs) = x : init' xs

pref :: [a] -> [[a]]
pref [] = [[]]
pref lst = [lst] ++ pref lst'
	where 
		lst' = init' lst 


segms :: [a] -> [[a]]
segms [] = []
segms lst = segms2 posts []
	where posts = post lst

segms2 :: [[a]] -> [[a]] -> [[a]]
segms2 [] res =  res
segms2 lst res = segms2 tail' (x ++ res)
	where 
		tail' = tail(lst)
		head' = head(lst)
		x = pref head'

permutation :: [a] -> [[a]]
permutation xs = doPerm xs []
	where 
		doPerm [] _ = [[]]
		doPerm [y] ys = (y:) <$> doPerm ys []
		doPerm (y : ys) zs = doPerm [y] (ys ++ zs) ++ doPerm ys (y : zs)
		    

