faktorial :: Integer -> Integer
faktorial 0 = 1
faktorial n = faktorial (n-1) * n

--faktorial' :: Integer -> Maybe Integer
--faktorial' n 
--	| n < 0 = Nothing
--	| n == 0 = Just 1
--	| otherwise = faktorial' (n-1) * n

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)	