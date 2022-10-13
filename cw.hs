---- Part 1a ----
-- Crypto by Galios
-- Cryptol is a language used for specifying cryptographic algorithms.
-- A cryptographic algorithms implemented using cryptol shows the mathematical specification of the algorithm
-- Cryptol enables the user to generate thier cryptographic algorithm and prove theorems based on thier algorithms and it allows other users to understand all notations anf implementation which occured inorder to create the algorithm
-- Originally, cryptol was designed for the use of the United States National Security Agency,
-- but it is also used in private companies such as Rockwell Collins (Aerospace and Defence Contracters) as well as in educative places.

-- Sigma by Facebook
-- Sigma was originally created using the FXL Language, but Facebook redesigned the software using haskell
-- Sigma is a system which is minimises the amount of spam, malware and other abuse which occurs on Facebook. 
-- It does this by proactively identify malicious actions on Facebook and automatically removes them so they do not appear on the users feed
-- These malicious actions are identified by policies which are written in Sigma. Every interaction on Facebook will follow with a policies evaluation by Sigma. The set of policies will depend on the interaction.
-- Due to the software being in haskell, Facebook are able to continuously oush out new policies or update previous ones made at a quick rate inorder to respond to new types of abuse on the socail media platform.

-- Haskell for supply chain optimization
-- Target, an american supermaerket, makes us of haskell for thier supply chain optimization
-- They have 37 distribution centers around the country and for that, they would need a software which controls how items move between distribution centers and thier stores
-- The software aims to maximising the items which are favourable for thier customers and minimise the cost that the business incurs.
-- Inorder to maximise which items are favourable, theier software makes uses of the Markov Decision Processes which is implemented in haskell. 
-- Since Haskell functions are very similar to mathematical functions, implement of this Markov Decision Processes would be much easier compared to other programming langauges.
-- This allows target to move thier items from thier distribution centers to thier stores at the most efficient way possible in order to reduce thier costs.

---- Part 1b ----

-- Functional programming leads to less bugs in programs. This is because each function is just a mapping of inputs to outputs. 
-- In order to find errors and bugs within program, a programmer can simply stack trace or print statement at each level to reveal the problems.
-- Since the output and inputs are always defined in the function's signature in haskell,
-- It would easy for a programmer can easily predict the output of the program for any given inputs. 
-- In addition to this, no hidden output can be produces as only the outputs defined in the signature will be shown to the users.

-- Functional programming make use of pure functions (its return values is the same for the same arguments and its evaluation has no side effects).
-- These pure functions are easier to test as the output are always the same for any given inputs. 

-- Although recursion is difficult to learn, it is much easier to implement in functional programming

-- The readability of your code would be much better as the need to iterative functions are not used in functional programming.

---- Part 1c ----

-- A function is essentailly a way of mapping an input to an output. There are different types of function which a have different mappings.
-- For example, some function have one-to-one mapping which means that each input has a unique output.
-- Functions in programming can be compared to mathematical functions in different ways, 
-- however Haskell functions are much closer to mathematical functions compared to other programming langauges
-- All basic mathematical functions can be performed by haskell functions due to the fact that haskell functions are pure. 
-- Even a list of the infinite set of numbers can be represented in haskell however, the list would never end since the set is infinite. 
-- In some mathematical functions, multiple outputs can be shown as different values. 
-- In contrast, haskell can only have one output but this can be solved by using tuples of a certain size to show the different outputs.
-- Moreover, Haskell function can accept inputs from different variable types including strings, and char. The creation of data types is also possible in haskell. 
-- In mathematical functions however, only integers and quotient numbers can be used as inputs
-- In conclusion, i believe that haskell functions do not fully resemble mathematical functions.
-- Thier similarities are very close together that it may be hard to distinguish between the two types of functions. 
-- In fact, some studies could show that haskell functions are mathematical functions which can aslo be applied to different types of input variables

---- Part 2a ----

type Dog = (String,Int) -- creates type dog which is a tuple containing a strign and integer

---- Part 2b ----

create_dog_list :: [String] -> [Int] -> [Dog]
create_dog_list [] [] = []
create_dog_list [x] [] = [(x,0)]    -- base case incase input is empty --
create_dog_list [] [y] = [("",y)]
create_dog_list (x:xs) (y:ys) = (x,y) : create_dog_list xs ys  

  -- this function adds the head of each list together in a tuple which is add to a list.
  -- The function then is recursively called until both lists are empty. 

---- Part 2c ----

sort_dog_list :: [Dog] -> [Dog]
sort_dog_list [] = []
sort_dog_list (x:xs) = sort_dog_list[y | y<-xs , snd y<= snd (x)] ++  -- snd gets the second element of a tuple.
				[x] ++ sort_dog_list[y | y<-xs , snd y>snd (x)]

  -- the function makes use of quick sort for its sorting method. all the tuples of the list excluding the first element is compared to the first.
  -- since the dogs have to be sorted by thier hieght, i made use of the snd function which get the second element of each tuple.
  -- each list comprehension is rescursively called this each side (less than first element and greater then first element) is sorted
  -- these lists are then added together.
 
---- Part 2d ----

remove_smallest_dogs :: Int -> [Dog] -> [Dog]
remove_smallest_dogs x [] = []
remove_smallest_dogs x y = drop x (sort_dog_list y)

  -- this function sorts out the list of dogs using the previous sort_dog_list function and drops however many dogs from the list as the user inputs.
  -- since the drop function take the first n elements, then the smallest does are removed

---- Part 2e ----

remove_tall_dogs :: [Dog] -> [Dog]
remove_tall_dogs [] = []
remove_tall_dogs x = [y | y<-x , snd y<=80]

  -- this function makes use of list comprehension inorder to remove any dogs in which thier height is greater than 80 cm. 
  -- Again i had to use the snd function to get the hieght of each dog out so it was comparable.

---- Part 3a ----

  -- m refers to the height of the steps
  -- n refers to the length of the steps
  -- p refers to the how many steps there are

character:: Int -> String
character n = (replicate n '*' ++ "\n") 

  -- function replicates the stars in the line by n times and adds a "\n" inorder to move to next line.

line :: Int -> String -> String
line m x = concat(replicate m x)

  -- this function replicates the hieght of the steps by m times

spaces :: Int -> String
spaces n =  concat(replicate (n) " ")

  -- this function regulates the amount of spaces before each line appears.
 
reverse2 :: Int -> Int -> Int -> String
reverse2 m n 0 = []
reverse2 m n p = (reverse2 m (n) (p-1)) ++ (line m (spaces (n*p) ++ character (n*p)))

  -- This function recursively calls itself till p is 0. once p is 0 then it add the strings together to create pattern.
  -- for example lets take m = 1, n = 3 and p = 4
  -- reverse2 1 3 0 ++ reverse2 1 3 1 ++ reverse2 1 3 2 ++ reverse2 1 3 3 ++ reverse2 1 3 4
  -- line above translates to :- [] ++ (line 1 (spaces (3*1) ++ character (3*1))) ++ (line 1 (spaces (3*2) ++ character (3*2))) 
												-- ++ (line 1 (spaces (3*3) ++ character (3*3))) ++ (line 1 (spaces (3*4) ++ character (3*4)))
notreverse2 :: Int -> Int -> Int -> String
notreverse2 m n 0 = []
notreverse2 m n p = line m (spaces (n*p) ++ character (n*p)) ++ notreverse2 m n (p-1)

steps :: Int -> Int -> Int -> String
steps m n 0 = []
steps m 0 p = []
steps 0 n p = reverse2 1 n p ++ notreverse2 1 n p
steps m n p = reverse2 m n p ++ notreverse2 m n p 

  -- Inorder to create this steps pattern, i had to make to functions (the reverse and the not reverse).
  -- The reverse function purpose is to make the the steps go from the smallest n to the largest n. 
  -- For example, lets say m = 1, n = 3 and p = 4. the reverse function would create a pattern so that the steps look like this:
  --   ***\n      ******\n         *********\n            ************\n
  -- The not reverse function does the opposite of the reverse. It goes from the largest n to the smallest n.
  -- The pattern would look like this:
  --            ************\n         *********\n      ******\n   ***\n 
  -- the reverse steps is then add to the not reverse steps to give the full steps pattern
		
---- Part 3b ----

makeflag :: Int -> Int -> Int -> [Char]
makeflag row column size =  concat (concat[[if row == size+1 then "\n"
					          else if row == 1 || column == 1 || row == size || column == size then "*"
					          else if elem True [(row == i || row == 1+size-i) && (column == i || column == 1+size-i)| i <- [1..size]] then "+"
					          else " " | row <- [1..size+1]] | column <- [1..size]])
							  
  -- This function places the stars and plus string in thier respectable column and rows in a matrix.
  -- Inorder to get the stars in correct place, the row should either be 1 or the size of matrix or the column should be 1 or size of matrix
  -- the pluses are always in position when the row equals to the column, or the column is equal to (size of matrix + 1) minus the row number and vice versa

flagpattern :: Int -> Int -> String
flagpattern n m = concat(replicate m (makeflag n n n))

 -- the flagpattern function just get the matrix from the makeflag function and add each row and column together so the output is a string.

---- Part 4 ----

check :: String -> String -> String
check n m = [x | x <-n, not (elem x m)] 

 -- check function removes any letter in the first string which is already in the second string.

lahi :: String -> [Char]
lahi x
	| length x == 0 || ((length x)+3) `mod` 4 == 0 = "l"
	| ((length x)+2) `mod` 4 == 0 = "a"
	| ((length x)+1) `mod` 4 == 0 = "h"
	| ((length x)) `mod` 4 == 0 = "i"
	
  -- lahi function returns the last lahi (like, admire, hates, indifferent) character when you assign each letter to a string by using a pattern.

ncompatm :: String -> String -> String
ncompatm n m 
	| lahi (check n m) == "l" = n ++ " likes " ++ m
	| lahi (check n m) == "a" = n ++ " admires " ++ m
	| lahi (check n m) == "h" = n ++ " hates " ++ m
	| lahi (check n m) == "i" = n ++ " is indifferent " ++ m

  -- ncompatm function just assign the result of lahi function to a worded way to match the function specifications.
  
	 
compatibility :: String -> String -> String
compatibility n m =  ncompatm n m ++ " and " ++ ncompatm m n

  -- compatibility function shows both the compatibility of n to m and vice versa

---- Part 5 ----

-- length2 :: [Int] -> Int
-- length2 n = length n

-- nsplit :: [a] -> a -> [Int]
-- nsplit a n 
	-- | head a == n = []
	-- | otherwise = head a : nsplit (tail a) n 

  -- i was not able to complete this function due to errors about polymorphisms



