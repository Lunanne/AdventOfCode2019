module Day6 (createTree, part1, reorderInput, part2, Tree) where

import Debug.Trace
import Data.List.Split
import Control.Monad.State
import Data.List

data Tree = Leaf | Node String Tree Tree deriving Show
data Direction = L | R deriving (Show)
type Directions = [Direction]

countConnections :: Tree -> Int -> Int
-- countConnections a b| trace ("countConnections tree" ++ show a ++ " count " ++ show b) False = undefined
countConnections Leaf count= 0
countConnections (Node _ leftSubtree rightSubtree) count = 
  count + (countConnections leftSubtree (count+1)) + (countConnections rightSubtree (count+1))

createTree :: [[String]] -> Tree -> Tree
-- createTree a b| trace ("create Tree " ++ show a) False = undefined
createTree input tree 
  | (length input) == 0 = tree
  | otherwise = createTree (tail input) newTree
  where
    parent = (head input) !! 0
    newNode = (head input) !! 1
    directions = findNode parent tree []
    newTree = addNode directions newNode tree 

findNode :: String -> Tree -> Directions -> Directions
findNode text (Node x Leaf Leaf) directions 
  | x == text = directions
  | otherwise = []
findNode text (Node x l Leaf) directions 
  | x == text = directions
  | otherwise = findNode text l (directions ++ [L])
findNode text (Node x Leaf r) directions 
  | x == text = directions
  | otherwise = findNode text r (directions ++ [R])
findNode text (Node x l r) directions 
  | x == text = directions
  | otherwise = (findNode text l (directions ++ [L])) ++ (findNode text r (directions ++ [R]))

createListOfNodes :: Directions -> Tree -> [Tree] -> [Tree]
-- createListOfNodes a b c| trace ("createListOfNodes directions " ++ show a ++ " tree " ++ show b) False = undefined
createListOfNodes (L:ds) (Node x l r) route = 
   (createListOfNodes (tail ds) l newRoute)
  where
    newRoute = route ++ [(Node x l r)]
createListOfNodes (R:ds) (Node x l r) route = 
  (createListOfNodes (tail ds) r newRoute)
  where
    newRoute = route ++ [(Node x l r)]
createListOfNodes [L] _ route = route
createListOfNodes [R] _ route = route
createListOfNodes [] _ route = route


addNode :: Directions -> String -> Tree -> Tree
-- addNode a b c| trace ("addNode directions " ++ show a ++ " text " ++ show b ++ " tree " ++ show c) False = undefined
addNode (L:ds) text (Node x l r) = Node x (addNode ds text l) r
addNode (R:ds) text (Node x l r) = Node x l (addNode ds text r)
addNode [] text (Node x l Leaf) =
  Node x l newPart
  where
    newPart = Node text Leaf Leaf
addNode [] text (Node x Leaf r) =
  Node x newPart r
  where
    newPart = Node text Leaf Leaf
addNode [] text (Node x Leaf Leaf) =
  Node x newPart Leaf
  where
    newPart = Node text Leaf Leaf

part1 :: [String] -> Int
part1 lines =
  countConnections tree 0
  where
    input = map (splitOn ")") lines
    reordered = reorderInput "COM" input []
    tree = createTree reordered (Node "COM" Leaf Leaf)

part2 :: [String] -> Int
part2 lines = (length notSharedYou) + (length notSharedSan) -2
  where
    input = map (splitOn ")") lines
    reordered = reorderInput "COM" input []
    tree = createTree reordered (Node "COM" Leaf Leaf)
    directionsYou = findNode "YOU" tree []
    directionsSan = findNode "SAN" tree []
    combined = zipWithPadding "" "" (map show directionsYou) (map show directionsSan)
    countShared = length (takeWhile (uncurry (==)) combined)
    notSharedYou = drop countShared directionsYou
    notSharedSan = drop countShared directionsSan


reorderInput :: String -> [[String]]-> [[String]] -> [[String]]
-- reorderInput a b c| trace ("reorder input " ++ show a ++ " new input " ++ show c) False = undefined
reorderInput search original newInput
  |(length original) == (length newInput) = newInput
  |otherwise = reorderInput text original modifiedInput
    where
      entries = filter (\a -> (a!!0) == search) original
      modifiedInput = newInput ++ entries
      notFoundYet = (filter (\a -> notElem a modifiedInput) original)
      mapNewInput0 = (map (\b -> b !!0) modifiedInput)
      mapNewInput1 = (map (\b -> b !!1) modifiedInput)
      text = if((length entries) /= 0) then (entries !! 0) !! 1 else ((filter (\a-> (elem (a!!0) mapNewInput0) || (elem (a!!0) mapNewInput1)) notFoundYet) !! 0) !! 0
     
zipWithPadding :: String -> String -> [String] -> [String] -> [(String,String)]
-- zipWithPadding a b c d| trace ("zipWithPadding " ++ show c ++ " santa " ++ show d) False = undefined
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)