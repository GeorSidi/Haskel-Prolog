--Georgios Sidiropulos 4488--
--boithitikes sinartiseis--
helpkgcd::Int->Int->Int->Int->Int
helpkgcd m n s j 
				|s==0 && j==1 = 0
				|s==0 && j==0 = 1
				|s==0 = -1
				|j==0 = s+1
				|mod m s == 0 && mod n s ==0 = helpkgcd m n (s-1) (j-1) 
				|otherwise = helpkgcd m n (s-1) j

helper::(Int->Int->Int->Int)->Int->Int->Int->Int
helper f a k b
			|k==b = f a k b
			|otherwise = f a k b + helper f a (k+1) b
--sinartiseis--
count :: Integer -> Int
count 0 = 0
count n
	| n < 0 = count(-n)
	| x == 0 || x==3 || x==6 || x==9 = 1 + count((div n 10))
	| otherwise = 0 + count((div n 10))
	where x=mod n 10

kgcd :: Int -> Int -> Int -> Int
kgcd m n k
			|k>n || k>m = -1
			|m < n =helpkgcd m n n k
			|otherwise = helpkgcd m n m k 

seconds:: (Int,Int)->(Int,Int,Int)->Int
seconds(d,m)(h,min,s)
				|d<1 || m <1 = -1 
				|d>31 || m>12 || h>23 || min >59 || s>59 = -1 
				|m==2 && d > 28 = -1
				|m==4 && d>30 || m==6 && d>30 || m ==9 && d>30 || m==11 && d>30 = -1
				|m==1 || m==2 =(m-1)*(31*86400) + (d-1)*86400 + (h*3600) + (min*60) + (s)
				|m==3 || m==4 =(m-1)*(31*86400) + (d-1)*86400 + (h*3600) + (min*60) + (s) - (3*86400)
				|m==5 || m==6 =(m-1)*(31*86400) + (d-1)*86400 + (h*3600) + (min*60) + (s) - (4*86400)
				|m==7 || m==8 || m==9 =(m-1)*(31*86400) + (d-1)*86400 + (h*3600) + (min*60) + (s) - (5*86400)
				|m==10 || m==11 =(m-1)*(31*86400) + (d-1)*86400 + (h*3600) + (min*60) + (s) - (6*86400)
				|otherwise =(m-1)*(31*86400) + (d-1)*86400 + (h*3600) + (min*60) + (s) -(7*86400)

sumfab::(Int->Int->Int->Int)->Int->Int->Int
sumfab f a b
			|a==b = 0 
			|a<b = f a a b + helper f a (a+1) b
			|otherwise = f a b b + helper f b (b+1) a
