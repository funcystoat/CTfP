{-
Write a function called add_and_double which adds two numbers together 
and then doubles the result.
-}
addAndDouble :: Num n => n -> n -> n
addAndDouble a b = 2*(a+b)

{-
Write an infix operator called +* which does the same thing as 
add_and_double. Define it in terms of add_and_double using the 
backtick (`) notation.
-}
(+*) :: Num a => a -> a -> a
(+*) a b = a `addAndDouble` b

{-
    Write a function called solve_quadratic_equation which takes in 
    three arguments (a, b, and c) which are coefficients to the 
    quadratic equation a x2 + b x + c = 0. a, b, c, and x should have 
    type Double. The output should be a tuple containing the two roots. 
    Don't worry about complex roots; if you apply the sqrt function to a negative number you will get NaN (Not A Number). Use a let or a where expression to define the square root of the discriminant (sqrt(b ** 2 - 4 * a * c)).
-}
quadraticRoots :: Floating f => f -> f -> f -> (f, f)
quadraticRoots a b c = (x_1, x_2)
    where
        x_1 = (-b + d)/(2*a)
        x_2 = (-b - d)/(2*a)
        d = sqrt(b**2 - 4*a*c)

{-
    Write a function called first_n which takes an Int (not an Integer!) 
    value (n) and returns a list of the first n Ints starting from 1. 
    In your solution, use an infinite list and the take function from 
    the Prelude (see here for documentation on the Prelude functions). 
    We use Ints rather than Integers because the take function's 
    first argument is of type Int.
-}
first_n :: Int -> [Int]
first_n n = take n [1..n]