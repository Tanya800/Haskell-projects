isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs
    
cross x y = crossing x y []

crossing [] _ z = z
crossing (x:xs) (y:ys) z = if (isMember x (y:ys)) then crossing xs (y:ys) (x:z)
        else crossing xs (y:ys) z

union [] y = y 
union (x:xs) y = if (isMember x y) then union xs y
        else union xs (x:y)
        
diff x y = difference x y []

difference [] _ z = z
difference (x:xs) y z = if (isMember x y) then difference xs y z
        else difference xs y (x:z)

main = print (diff [7,2] [1,2,4])