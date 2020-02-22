module Huffman where

import Data.List

data HuffmanTree = HuffmanLeaf Char | HuffmanBranch HuffmanTree HuffmanTree deriving (Show)
data WeightedTree = WeightedLeaf Integer Char | WeightedBranch Integer WeightedTree WeightedTree deriving (Show)

statistics :: String -> [(Integer, Char)]
statistics str = map (\x -> (genericLength x, head x)) (group (str))

makeTree :: String -> HuffmanTree
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

removeWeightsFromTree (WeightedLeaf _ char) = HuffmanLeaf char
removeWeightsFromTree (WeightedBranch _ w1 w2) = HuffmanBranch (removeWeightsFromTree w1) (removeWeightsFromTree w2)


encode :: String -> (HuffmanTree , [Integer])
encode str = ((makeTree str), (encodeString str (makeTree str)))

encodeString [] tree = []
encodeString (x:xs) tree = encodeStringHelper x tree ++ encodeString xs tree

encodeStringHelper currentEncodingChar (HuffmanBranch left right) = 
    if ((checkIfTreeContainsChar currentEncodingChar left) == True)
        then 0 : encodeStringHelper currentEncodingChar left
        else 1 : encodeStringHelper currentEncodingChar right

encodeStringHelper currentEncodingChar (HuffmanLeaf char) = [] 

checkIfTreeContainsChar searchChar (HuffmanBranch left right) =
    if (checkIfTreeContainsChar searchChar left)
        then True
        else checkIfTreeContainsChar searchChar right

checkIfTreeContainsChar currentEncodingChar (HuffmanLeaf char) = (currentEncodingChar == char)




decode :: HuffmanTree -> [Integer] -> String
decode tree bitStr = decodeHelper tree tree bitStr

--decodeHelper :: HuffmanTree -> HuffmanTree -> String -> String
decodeHelper startTree (HuffmanLeaf str) [] = str
decodeHelper startTree (HuffmanLeaf str) bitStr = str : decodeHelper startTree startTree bitStr

decodeHelper startTree (HuffmanBranch left right) (x:bitStrRest) =
    if (x == 0)
        then decodeHelper startTree left bitStrRest
        else decodeHelper startTree right bitStrRest