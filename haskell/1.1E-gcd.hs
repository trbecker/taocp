{-
TAOCP Algorithm 1.1E
This algorithm calculates the greatest common divider for two positive integers m and n.
-}
gcd' :: (Integral a) => a -> a -> a
{-
    This algorithm is clearly defined for positive integrals. There's no universal agreement on
	what is the greatest common divider if one or both of the arguments are negative, and there's
	no reminder on division with Reals.
	If more robusteness is required, you may want to uncoment the line of code before E0.
	
	This algorithm is proven finite because r will always be lower than n (if you disagree, you should
	take a look on the properties of division of integers on wikipedia.)
-}
gcd' m n
{-
	| (n <= 0) || (m <= 0) = error "Both arguments must be positive integers"
-}
	{- E0. [Ensure n <= m.] If n > m, exchange n <-> m -}
    | n > m = gcd' n m
	{- E1. [Find reminder.] Divide m by n and let r be the reminder (see the where clause) -}
	{- E2. [Is reminder zero?] If r = 0, the algorithm terminates; the answer is n -}
    | r == 0 = n
	{- E3. [Reduce.] Let m <- n, n <- r, and go back to step one. -}
	| otherwise = gcd' n r
	where r = m `mod` n