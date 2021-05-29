transp ([]:_) = []
transp matrix = map head matrix : transp (map tail matrix)

-- version 1
multimatrix a b = ml a (transp b)

ml [] _ = []
ml a b =
	let 
		na = head a
		ta = tail a
	in mulmat na b : ml ta b 

mulmat _ [] = [] -- умножение вектора a на матрицу b 
mulmat a b = 
		let
			hb = head b 
			tb = tail b
		in 
		(\ x y -> sum $ zipWith (*) x y) a hb : mulmat a tb 


-- version 2


multimatrix a b = mulmatrix a $ transp b
mulmatrix [] _ = []
mulmatrix (a:as) b =  (map sum $ map ( zipWith (*) a ) b ) : mulmatrix as b

