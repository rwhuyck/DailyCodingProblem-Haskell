module DailyCodingProblem3(main) where
{- 
This problem was asked by Google.

Given the root to a binary tree, implement serialize(root), 
which serializes the tree into a string, and deserialize(s), 
which deserializes the string back into the tree.

For example, given the following Node class

class Node:
def __init__(self, val, left=None, right=None):
self.val = val
self.left = left
self.right = right

The following test should pass:

node = Node('root', Node('left', Node('left.left')), Node('right'))
assert deserialize(serialize(node)).left.left.val == 'left.left'
-}
import Data.List.Split

--Tree definition from https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/
data BinaryTree a = EmptyNode
    |Node a (BinaryTree a) (BinaryTree a)
    deriving (Eq,Ord,Show,Read)

fillTree :: [String] -> Int -> BinaryTree String
fillTree [] n = EmptyNode
fillTree xs n 
    |n >= length xs = EmptyNode
    |otherwise = Node (xs !! n) (fillTree xs (n * 2 + 1)) (fillTree xs (n * 2 + 2))

serializeTree :: (Show a) => BinaryTree a -> String
serializeTree EmptyNode = ""
serializeTree (Node a left right) = show a ++ (serializeTree left) ++ (serializeTree right)    

input = "This is a sentence and soon a tree as well."
splitWords = splitOn " " input

myTree = fillTree splitWords 0
serialized = serializeTree myTree
result = putStrLn serialized

main = result
