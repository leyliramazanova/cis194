module Week8.Party where
import Week8.Employee
import Data.Monoid
import Data.Tree
import Data.List


{-
Exercise 1
Now define the following tools for working with GuestLists:
1. A function
glCons :: Employee -> GuestList -> GuestList
which adds an Employee to the GuestList (updating the cached Fun score appropriately). 
Of course, in general this is impossible: the updated fun score should depend on whether the Employee being added is already in the list, 
or if any of their direct subor- dinates are in the list, and so on. For our purposes, though, you may assume that none of these special cases will hold: 
that is, glCons should simply add the new Employee and add their fun score without doing any kind of checks.
2. A Monoid instance for GuestList. (How is the Monoid instance supposed to work, you ask? You figure it out!)
3. A function moreFun :: GuestList -> GuestList -> GuestList 
which takes two GuestLists and returns whichever one of them is more fun, i.e. has the higher fun score. \
(If the scores are equal it does not matter which is returned.)
-}

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emp_ls f) = GL (emp_ls ++ [emp]) (f + empFun emp)

instance Semigroup GuestList where
    (<>) (GL emp_ls1 fun1) (GL emp_ls2 fun2)= GL (emp_ls1 ++ emp_ls2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0
    GL emp_ls1 fun1 `mappend` GL emp_ls2 fun2 = GL (emp_ls1 ++ emp_ls2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL emp_ls1 fun1) (GL emp_ls2 fun2) 
    | fun1 >= fun2 = GL emp_ls1 fun1
    | otherwise = GL emp_ls2 fun2

{-
Exercise 2
The Data.Tree module from the standard Haskell libraries defines
the type of “rose trees”, where each node stores a data element and has any number of children (i.e. a list of subtrees):
data Tree a = Node {
rootLabel :: a, -- label value
subForest :: [Tree a] -- zero or more child trees}
Strangely, Data.Tree does not define a fold for this type! Rectify the situation by implementing
treeFold :: ... -> Tree a -> b
(See if you can figure out what type(s) should replace the dots in the type of treeFold. 
    If you are stuck, look back at the lecture notes from Week 7, or infer the proper type(s) from the remainder of this assignment.)
The algorithm
Now let’s actually derive an algorithm to solve this problem. 
Clearly there must be some sort of recursion involved—in fact, it seems that we should be able to do it with a fold. 
This makes sense though— starting from the bottom of the tree and working our way up, we compute the best guest list for each subtree and somehow combine these to decide on the guest list for the next level up, and so on. 
So we need to write a combining function
combineGLs :: Employee -> [GuestList] -> GuestList
which takes an employee (the boss of some division) and the optimal guest list for each subdivision under him, and somehow combines this information to compute the best guest list for the entire division.
However, this obvious first attempt fails! 
The problem is that we don’t get enough information from the recursive calls. If the best guest list for some subtree involves inviting that subtree’s boss, 
then we are stuck, since we might want to consider inviting the boss of the entire tree—in which case we don’t want to invite any of the subtree bosses 
(since they wouldn’t have any fun anyway). But we might be able to do better than just taking the best possible guest list for each subtree and then excluding their bosses.
The solution is to generalize the recursion to compute more infor- mation, in such a way that we can actually make the recursive step. 
In particular, instead of just computing the best guest list for a given tree, we will compute two guest lists:
1. the best possible guest list we can create if we invite the boss (that is, the Employee at the root of the tree); and
2. the best possible guest list we can create if we don’t invite the boss. It turns out that this gives us enough information at each step to
compute the optimal two guest lists for the next level up.
-}

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r []) = f r $ []
treeFold f (Node r ls) = f r $ (treeFold f <$> ls)


combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs emp guestList = foldr moreFun (GL [emp] $ empFun emp) guestList

{-
Exercise 3
Write a function
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
which takes two arguments. The first is the “boss” of the current sub- tree (let’s call him Bob). 
The second argument is a list of the results for each subtree under Bob. 
Each result is a pair of GuestLists: the first GuestList in the pair is the best possible guest list with the boss of that subtree; 
the second is the best possible guest list without the boss of that subtree. 
nextLevel should then compute the overall best guest list that includes Bob, and the overall best guest list that doesn’t include Bob.
-}

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel _ ls = (bestGLwith, bestGLwithout)
    where 
        bestGLwith =  foldMap fst ls
        bestGLwithout = foldMap snd ls

{-
Exercise 4
Finally, put all of this together to define
maxFun :: Tree Employee -> GuestList
which takes a company hierarchy as input and outputs a fun-maximizing guest list. 
You can test your function on testCompany, provided in Employee.hs.
The whole company
Of course, the actual tree of employees in your company is much larger! 
We have provided you with a file, company.txt, containing the entire hierarchy for your company. 
The contents of this file were created by calling the show function on a Tree Employee,3 so you can convert it back into a Tree Employee using the read function.
-}

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

{-
Exercise 5
Implement main :: IO () so that it reads your company’s hierarchy from the file company.txt, and then prints out a formatted guest list, sorted by first name, which looks like
Total fun: 23924 Adam Debergues Adeline Anselme ...
(Note: the above is just an example of the format; it is not the correct output!) 
You will probably find the readFile and putStrLn functions useful.
As much as possible, try to separate out the “pure” computation from the IO computation. 
In other words, your main function should actually be fairly short, calling out to helper functions (whose types do not involve IO) to do most of the work. 
If you find IO “infecting” all your function types, you are Doing It Wrong.
-}

empFormat :: [Employee] -> String
empFormat = unlines . sort . map empName

totalFormat :: GuestList -> String
totalFormat (GL emp_ls f) = "Total fun:" ++ show f ++ "\n" ++ empFormat emp_ls

main :: IO ()
main = readFile "company.txt" >>= putStrLn . totalFormat . maxFun . read

