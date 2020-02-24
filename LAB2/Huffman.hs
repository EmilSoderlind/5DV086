module Huffman where

import Data.List

data Htree = Leaf Char | Branch Htree Htree deriving (Show)
data WeightedTree = WeightedLeaf Integer Char | WeightedBranch Integer WeightedTree WeightedTree deriving (Show)

-- Returns list of tupples with number of occurency of each characher in the provided string.
statistics :: String -> [(Integer, Char)]
statistics str = map (\x -> (genericLength x, head x)) (group (sort str))

-- Creates a huffman-tree for a given string.
makeTree :: String -> Htree
makeTree str = removeWeightsFromTree $ combineTrees $ makeSortedTree(str)

makeTreeHelper :: String -> [WeightedTree]
makeTreeHelper str = map (\x -> (WeightedLeaf (fst x) (snd x))) (statistics str)

makeSortedTree :: String -> [WeightedTree]
makeSortedTree str = sortTreeList(makeTreeHelper str)

weightOfTree :: WeightedTree -> Integer
weightOfTree (WeightedLeaf weight _) = weight
weightOfTree (WeightedBranch weight _ _) = weight

sortTreeList :: [WeightedTree] -> [WeightedTree]
sortTreeList list = sortBy sortTreesHelper list 

sortTreesHelper w1 w2
    | weightOfTree w1 < weightOfTree w2 = LT
    | weightOfTree w1 >= weightOfTree w2 = GT

combineTrees (f:[]) = f
combineTrees (f:s:[]) = ((WeightedBranch (weightOfTree f + weightOfTree s) f s))
combineTrees (f:s:rest) = combineTrees $ sortTreeList ((WeightedBranch (weightOfTree f + weightOfTree s) f s) : sortTreeList rest)

removeWeightsFromTree (WeightedLeaf _ char) = Leaf char
removeWeightsFromTree (WeightedBranch _ w1 w2) = Branch (removeWeightsFromTree w1) (removeWeightsFromTree w2)


-- Huffman encode a provided String. Returns the created huffman-tree and the huffman-encoding of the provided String.
encode :: String -> (Htree , [Integer])
encode [] = (Leaf ' ', [])
encode str = ((makeTree str), (encodeString str (makeTree str)))

encodeString str (Leaf char) = replicate (length str) 0
encodeString [] tree = []
encodeString (x:xs) tree = encodeStringHelper x tree ++ encodeString xs tree

encodeStringHelper currentEncodingChar (Branch left right) = 
    if ((checkIfTreeContainsChar currentEncodingChar left) == True)
        then 0 : encodeStringHelper currentEncodingChar left
        else 1 : encodeStringHelper currentEncodingChar right

encodeStringHelper currentEncodingChar (Leaf char) = []

checkIfTreeContainsChar searchChar (Branch left right) =
    if (checkIfTreeContainsChar searchChar left)
        then True
        else checkIfTreeContainsChar searchChar right

checkIfTreeContainsChar currentEncodingChar (Leaf char) = (currentEncodingChar == char)



-- Decode a huffman-encoding given the code and the huffman-tree.
decode :: Htree -> [Integer] -> String
decode (Leaf char) bitStr = replicate (length bitStr) char
decode tree bitStr = decodeHelper tree tree bitStr

decodeHelper startTree (Leaf str) [] = [str]
decodeHelper startTree (Leaf str) bitStr = str : decodeHelper startTree startTree bitStr

decodeHelper startTree (Branch left right) (x:bitStrRest) =
    if (x == 0)
        then decodeHelper startTree left bitStrRest
        else decodeHelper startTree right bitStrRest