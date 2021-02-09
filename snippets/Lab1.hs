module Lab1 where


circArea r = pi*r**2

mean a b = (a + b)/2

fizzBuzz :: Int -> String
fizzBuzz n | n `mod` 3 == 0 = "Fizz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 15 == 0 = "FizzBuzz"
           | otherwise = show n

gauss f a b = (b-a)/18*(5*g(-sqrt 0.6) + 8*g 0 + 5*g(sqrt 0.6))
  where g x = f $ (a+b)/2 + x*(b-1)/2
