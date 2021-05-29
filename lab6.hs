type Row = [Double] 
type Matrix = [Row] 
transpose_matrix2::Matrix->Matrix
transpose_matrix2 m = foldr1 (zipWith (++)) (map ( map (\ y -> [y]) ) m)




transp [] heads  ([]:_) = [reverse heads ]
transp [] heads tails = hds :  transp (reverse tails) [] []
 						where 
 							hds = reverse heads 
	
transp (l:ls) heads tails = transp ls (hh :heads) (th: tails)
	where  
		th = tail l
		hh = head l

transp1 ([]:_)=[]
transp1 matrix = map head matrix: transp1 nm
	where 
		nm = map tail matrix

