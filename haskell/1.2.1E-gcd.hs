{-
	TAOCP 1.2.1E - Greastest Common Divisor (extended)
	The algorithm will be implemented in two different functions, since there's 4 internal parameters
	(see step E1.) that doesn't need to be visible to the end user.
-}
gcd' :: (Integral a) => a -> a -> (a, a, a)

gcd' m n
{- Extra safety -}
	| (m <= 0) || (n <= 0) = error "Both m and n must be positive integers"
{-
	You will not find E0 in the original algorithm description, but I find that it is a useful safety measure.
	Also see the function reverseAB in this module.
-}
{- E0. [Ensure n <= m] If n > m, exchange n <-> m -}
	| n > m = reverseAB (gcd' n m)
{- E1. [Initialize.] Set a' <- b <- 1, a <- b' <- 0, c <- m, d <- n -}
	| otherwise = gcd'' 0 1 1 0 m n

gcd'' :: (Integral a) => a -> a -> a -> a -> a -> a -> (a, a, a)
gcd'' a b a' b' c d
{- E2. [Divide] Let q and r be the quotient and reminder of the division of c by d. (Note: see the where clause.) -}
{- E3. [Remainder zero?] If r = 0, the algorithm ends; the answer is a, b and d. -}
	| r == 0 = (a, b, d)
{- 
   E4. [Recycle.] Set c <- d, d <- r, t <- a', a' <- a, a <- t - qa, t <- b', b' <- b, b <- t - qb, and go back to E2.
-}
	| otherwise = gcd'' (a' - q * a) (b' - q * b) a b d r	
    where r = c `mod` d
          q = c `div` d
		  
reverseAB :: (a, a, a) -> (a, a, a)
reverseAB (x, y, z) = (y, x, z)