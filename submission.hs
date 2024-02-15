-- Creates the Algebraic data type 'Mytree', to represent a quadtree
data MyTree = White | Black | Branch MyTree MyTree MyTree MyTree deriving (Eq, Show)

--Functions to create instances of MyTree
allBlack :: Int -> MyTree
allBlack x = Black

allWhite :: Int -> MyTree
allWhite x = White

-- Function which creates a branch wth clockwise arrangement of subtrees
clockwise :: MyTree -> MyTree -> MyTree -> MyTree -> MyTree
clockwise a b c d = Branch a b c d

-- Function which creates a branch wth anti-clockwise arrangement of subtrees
anticlockwise :: MyTree -> MyTree -> MyTree -> MyTree -> MyTree
anticlockwise a b c d = Branch a d c b

-- Function which converts the quadtree to a list of tuples with coordinates.
quadToList :: MyTree -> (Double, (Double, Double)) -> [((Double, (Double, Double)), MyTree)]
quadToList Black (side_length, (x, y)) = [((side_length, (x, y)), Black)]
quadToList White (side_length, (x, y)) = [((side_length, (x, y)), White)]
quadToList (Branch a b c d) (side_length, (x, y)) = 
    quadToList a (side_length / 2, (x - side_length / 4, y + side_length / 4)) ++
    quadToList b (side_length / 2, (x + side_length / 4, y + side_length / 4)) ++
    quadToList c (side_length / 2, (x + side_length / 4, y - side_length / 4)) ++
    quadToList d (side_length / 2, (x - side_length / 4, y - side_length / 4))

-- Function which calculates the depth of a quadtree
treeDepth :: MyTree -> Double
treeDepth White = 0
treeDepth Black = 0
treeDepth (Branch t1 t2 t3 t4) = 1 + (max (treeDepth t1) (max (treeDepth t2) (max (treeDepth t3) (treeDepth t4))))

-- This function compares all elements with all other elements in the quadtree and checks if they are neighbours. 
-- It returns an int depending on the number of like and opposite neighbours. 
compareAdjacentNeighbours :: ((Double, (Double, Double)), MyTree) -> [((Double, (Double, Double)), MyTree) ]-> Int
compareAdjacentNeighbours ((side_length, (x, y)), q1)  [] = 0
compareAdjacentNeighbours ((side_length, (x, y)), q1) (((side_length1, (x1, y1)), q2) : l) =

    --Compares if the adjacent neighbour has the same colour and returns 1 if true
    if ( (((abs (x1 - x) == (side_length + side_length1)/2 )&& ((side_length >= side_length1 && y1 <= y+side_length/2-side_length1/2 && y1 >= y-side_length/2+side_length1/2) || (side_length < side_length1 && y <= y1+side_length1/2-side_length/2 && y >= y1-side_length1/2+side_length/2)))
    ||
    
    (abs(y1 - y) == (side_length + side_length1)/2) && ((side_length >= side_length1 && x1 <= x+side_length/2-side_length1/2 && x1 >= x-side_length/2+side_length1/2) || (side_length < side_length1 && x <= x1+side_length1/2-side_length/2 && x >= x1-side_length1/2+side_length/2)) )) && (q1 == q2)
        then 1 + compareAdjacentNeighbours ((side_length, (x, y)), q1)  l
    
    --Compares if the adjacent neighbour has the opposite colour and returns -1 if true
    else if ( (((abs (x1 - x) == (side_length + side_length1)/2 )&& ((side_length >= side_length1 && y1 <= y+side_length/2-side_length1/2 && y1 >= y-side_length/2+side_length1/2) || (side_length < side_length1 && y <= y1+side_length1/2-side_length/2 && y >= y1-side_length1/2+side_length/2)))
    ||
    
    (abs(y1 - y) == (side_length + side_length1)/2) && ((side_length >= side_length1 && x1 <= x+side_length/2-side_length1/2 && x1 >= x-side_length/2+side_length1/2) || (side_length < side_length1 && x <= x1+side_length1/2-side_length/2 && x >= x1-side_length1/2+side_length/2)) )) && (q1 /= q2)
        then -1 + compareAdjacentNeighbours ((side_length, (x, y)), q1)  l
    
    --returns 0 if it is not an adjacent neighbour
    else 0 + compareAdjacentNeighbours ((side_length, (x, y)), q1)  l

-- This function calls compareAdjacentNeighbours and flips the element if the value returned is less than 1.  
transfer :: [((Double, (Double, Double)), MyTree)] -> [((Double, (Double, Double)), MyTree)] -> [((Double, (Double, Double)), MyTree)]
transfer [] _ = []  -- Base case: empty list, no transfer possible
transfer (((x, (y, z)), q): l) l1 =
    -- checks if the node needs to be transferred/flipped based on adjacent neighbours
    if compareAdjacentNeighbours ((x, (y, z)), q) l1 < 0 
        then [((x, (y, z)), change q)] ++ transfer l l1
    else [((x, (y, z)), q)] ++ transfer l l1

-- This function changes the colour of the node
change :: MyTree -> MyTree
change White = Black
change Black = White

-- This function takes the updated, flipped list along with the side length and coordinates of origin, 
-- and puts a node in the quadtree in the specified position. 
putNode :: ((Double, (Double, Double)), MyTree) -> Double -> (Double, Double) -> MyTree -> MyTree
putNode ((side, (x, y)), q) s (m,n) p
    | (p == White || p == Black) && side == s = q
    | otherwise = 
        case p of
            Branch a b c d ->
                if x - m < 0 && y - n > 0
                    then Branch (putNode ((side, (x, y)), q) (s / 2) (m - s / 4, n + s / 4) a) b c d
                else if x - m > 0 && y - n > 0
                    then Branch a (putNode ((side, (x, y)), q) (s / 2) (m + s / 4, n + s / 4) b) c d
                else if x - m > 0 && y - n < 0
                    then Branch a b (putNode ((side, (x, y)), q) (s / 2) (m + s / 4, n - s / 4) c) d
                else if x - m < 0 && y - n < 0
                    then Branch a b c (putNode ((side, (x, y)), q) (s / 2) (m - s / 4, n - s / 4) d)
                else q
            _ -> q

-- Converts the list with coordinates back into a quadtree
makeQuad :: [((Double, (Double, Double)), MyTree)] -> MyTree -> MyTree
makeQuad [] pq = pq
makeQuad (x:l) pq = makeQuad l (putNode x (2**(treeDepth pq)) (0,0) pq) 

-- This function applies the blur function to the given tree
blur :: MyTree -> MyTree
blur White = White
blur Black = Black
blur (Branch a b c d) = 
  makeQuad (transfer (quadToList (Branch a b c d) ((2**treeDepth (Branch a b c d)),(0,0) )) (quadToList (Branch a b c d) ((2**treeDepth (Branch a b c d)),(0,0) ) )) (Branch a b c d)
