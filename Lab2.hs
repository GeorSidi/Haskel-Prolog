--Sidiropoulos Georgios-- --4488--
-----------------------------------------------------------------------------------------

-- ASKHSH 1
helper1::[Int]->Int->Int->Int->Int->Int
			
helper1 (s:t) n k l m
					|abs(s-n) < abs(m-n) = helper1 t n (k+l+1) 0 s
					|otherwise = helper1 t n k (l+1) m
helper1 [] n k l m = k - 1
					
			
nearest :: [Int]->Int->Int

nearest s n
			|length(s)==1 = 1
			|otherwise = helper1 s n 1 0 ((head(s))-n)
-----------------------------------------------------------------------------------------   
-- ASKHSH 2
helper2::[Int]->Int->Int

helper2 s k
		|k==0 = 0
		|otherwise = head(s) + helper2(tail(s)) (k-1)

smooth :: [Int]->Int->[Int]

smooth s k 
			|length(s)<k = []
			|otherwise = [(div (helper2(s) k ) k)] ++ smooth (tail(s)) k
-----------------------------------------------------------------------------------------
     
-- ASKHSH 3
--string = [Char]

--helper3::String->String

swap :: String->String

swap s
		|length(split s 1) `mod` 2 == 0 = tail(conbiner(tail(reverser(split s 1) 1)))
		|otherwise = tail(conbiner(reverser(split s 1) 1))

conbiner::[String]->String
conbiner []=[]
conbiner (s:t)=conbiner(t) ++ " " ++ s
				
reverser::[String]->Int->[String]
reverser s k
			|length(s)<1 = []
			|length(s)==1 = [head(s)]
			|k==1 = reverser (tail(s)) 0 ++  [head(s)] ++ [head(tail(s))]
			|otherwise =reverser (tail(s)) 1

split::String->Int->[String]
split [] k = []
split s k
			|length(s)<1 = []
			|[head(s)]==" " = split (tail(s)) 1
			|k==1 = [spliter(s)] ++ split (tail(s)) 0
			|otherwise = split (tail(s)) 0

spliter::String->String
spliter [] = []
spliter (s:t)
		|[s] == " " = []
		|otherwise = [s] ++ spliter(t)

-----------------------------------------------------------------------------------------
     
-- ASKHSH 4
helper4::[u]->(u->Int->v)->Int->[v]

helper4 [] f i = []
helper4 s f i = [f (head(s)) i] ++ helper4(tail(s)) f (i+1)

mapi :: [u]->(u->Int->v)->[v]

mapi s f = [] ++ helper4 s f 1




