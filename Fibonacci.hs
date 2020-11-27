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
readW ((y:ys),_) = y

-- wheel containing no elements
emptyW :: Wheel a
emptyW = ([],[])

-- test if a wheel is empty 
--isEmptyW :: Wheel a -> Bool
--isEmptyW w = let w1 = emptyW
--             in
--               if w == w1 then True
--                          else False 

-- move the head to the next element clockwise
--rightW :: Wheel a -> Wheel a

-- move the head to the next element anti-clockwise
--leftW :: Wheel a -> Wheel a

-- insert a new element the the left of the head and set as new head
--insertW :: a -> Wheel a -> Wheel a

-- extract and delete the head,  move the head to the next right
--extractW :: Wheel a -> (a, Wheel a)

-- concatenate two wheels
--   the new head is the head of the first (if non-empty)
--concatW :: Wheel a -> Wheel a -> Wheel a


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


-- the Fibonacci heap with no elements
--emptyFH :: FibHeap a

-- test if a heap is empty
--isEmptyFH :: FibHeap a -> Bool

-- Reading the minimum element
--  We assume that the head is heap-ordered,
--  so the minimum is the head of the root wheel
--minimumFH :: FibHeap a -> a

-- Inserting a new element into the heap
--insertFH :: Ord a => a -> FibHeap a -> FibHeap a

-- Merging two Fibonacci Heaps
--unionFH :: Ord a => FibHeap a -> FibHeap a -> FibHeap a

-- Extracting the minimum from a heap
--extractFH :: Ord a => FibHeap a -> (a,FibHeap a)