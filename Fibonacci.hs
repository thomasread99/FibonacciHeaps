{-
  G54PAD Project in Advanced Algorithms and Data Structures
    Autumn 2020

  Assignment 3 
    Fibonacci Heaps

  Student Name: Thomas Read
  Student ID: 4296726

  Complete this Haskell file by providing definitions
  of the following functions:

  Operations on Wheels:
  readW, emptyW, isEmptyW, rightW, LeftW,
  insertW, extractW, concatW

  Operations on Fibonacci Heaps:
  emptyFH, isEmptyFH, minimumFH,
  insertFH, unionFH, extractFH

  You are allowed to define any other auxiliary function you need.

-}


module Fibonacci where

import Data.Map

-- WHEELS

-- Double linked circular lists
-- Implemented so all operations have amortized complexity O(1)

type Wheel a = ([a],[a])

{- A pair ([y1,..,yn],[z1,..,zm]) represents a circular lists
   with elements [y1,..,yn,zm,..,z1];
   the element to the right of z1 is y1, the element to the left of y1 is z1.
   The head element is y1
-}

-- read the head element
readW :: Wheel a -> a
readW ((x:xs),_) = x

-- wheel containing no elements
emptyW :: Wheel a
emptyW = ([],[])

-- test if a wheel is empty 
isEmptyW :: Wheel a -> Bool
isEmptyW (x,y) = if Prelude.null x && Prelude.null y then True
                                     else False              

-- move the head to the next element clockwise
rightW :: Wheel a -> Wheel a
rightW ((x:xs),y) = (xs, [x] ++ y) 

-- move the head to the next element anti-clockwise
leftW :: Wheel a -> Wheel a
leftW (x,(y:ys)) = ([y] ++ x, ys)

-- insert a new element the the left of the head and set as new head
insertW :: a -> Wheel a -> Wheel a
insertW n (x,y) = ([n] ++ x, y)

-- extract and delete the head,  move the head to the next right
extractW :: Wheel a -> (a, Wheel a)
extractW ((x:xs),y) = (x, ((xs),y))

-- concatenate two wheels
--   the new head is the head of the first (if non-empty)
concatW :: Wheel a -> Wheel a -> Wheel a
concatW n ([],[]) = n
concatW ([],[]) n = n
--concatW (x,y) (x2,y2) = (x ++ reverse y, y2 ++ reverse x2)
concatW (x,y) (x2,y2) = (x ++ y, x2 ++ y2)


-- FIBONACCI HEAPS
{-
  A FH is a min-ordered tree consisting of
  a wheel of nodes each having a subtree
  Each node contains its degree
  We also keep track of the number of elements of the heap
-}

data FibHeap a = FHeap Int (Wheel (FHNode a))
  deriving Show

-- FHeap n w: a heap with n elements, consisting of a root wheel w

type FHNode a = (a, Int, FibHeap a)

-- (x,d,h) is an element with value x, degree d and sub-heap h

type NArray a = Map Int (FHNode a) 

-- the Fibonacci heap with no elements
emptyFH :: FibHeap a
emptyFH = FHeap 0 ([],[])

-- test if a heap is empty
isEmptyFH :: FibHeap a -> Bool
isEmptyFH (FHeap n (x,y)) = if Prelude.null x && Prelude.null y then True
                                                else False

-- Reading the minimum element
--  We assume that the head is heap-ordered,
--  so the minimum is the head of the root wheel
minimumFH :: FibHeap a -> a
minimumFH (FHeap n (x,y)) = extractFirst(head x)

extractFirst :: (a,b,c) -> a
extractFirst (a,_,_) = a

-- Inserting a new element into the heap
insertFH :: Ord a => a -> FibHeap a -> FibHeap a
insertFH x h@(FHeap n w) = if (isEmptyW w) then (FHeap 1 ([(x,0,emptyFH)],[]))
                                           else if x <= minimumFH h then (FHeap (n+1) (insertW (x,0,emptyFH) w))
                                                                    else (FHeap (n+1) (rightW (insertW (x,0,emptyFH) w)))                             

-- Merging two Fibonacci Heaps
unionFH :: Ord a => FibHeap a -> FibHeap a -> FibHeap a
unionFH h1@(FHeap n1 w1) h2@(FHeap n2 w2) = if isEmptyFH h1 then h2
                                                            else if isEmptyFH h2 then h1
                                                                                 else if minimumFH h1 <= minimumFH h2 then (FHeap (n1+n2) (concatW w1 w2))
                                                                                                                      else (FHeap (n1+n2) (concatW w2 w1))

-- Extracting the minimum from a heap
{-extractFH :: Ord a => FibHeap a -> (a,FibHeap a)
-- Add if the minimum was the only element of the wheel
extractFH (FHeap n w) = let ((x, FHeap nx wx), w') = extractW w
                        in (x, consolidate (FHeap n (concatW wx w')))-}

-- Auxiliary function to link two nodes
link :: FHNode a -> FHNode a -> FHNode a
link x@(kx,dx,hx) y@(ky,dy,hy) = if kx <= ky then (kx, dx+1, (FHeap 1 (insertN y hx)))
                                             else (ky, dx+1, (FHeap 1 (insertN x hy)))


 {- to insert a node into heap and return a wheel, that node can contain multiple elements
insertN (1,0, FHeap 0 ([],[])) h-}
insertN :: FHNode a -> FHeap a -> Wheel a 
insertN x h@(FHeap n w) = if (isEmptyW w) then ([x],[])
                                          else --Compare the node to the minimum in the heap and insert as appropriate

-- Auxiliary function to insert a new node into NArray
insNA :: FHNode a -> NArray a -> NArray a
insNA x@(kx,dx,hx) m = if isNothing (Data.Map.lookup dx m) then insert dx x m
                                                           --Complete this when link is finished                                                           
                                                           else m

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

test :: NArray a -> NArray a
test m = m

-- Auxiliary function to transform a wheel of nodes into an array
--makeNA :: (Wheel (FHNode a)) -> NArray
--makeNA w = if (isEmptyW w) then NArray.empty
--                           else let (x,w') = extractW w
--                                in NArray.empty
{-
TEST DATA:
([2,3,5],[8,4,0])
-}