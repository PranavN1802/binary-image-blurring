# QuadTree Representation in Haskell

This project defines a quad tree structure in Haskell using the `MyTree` data type, with nodes being either `White`, `Black`, or a `Branch` of four subtrees. Key functions allow creation, manipulation, and transformation of the quad tree.

## Data Structure

- **`MyTree`**: 
  - `White` - A white leaf node.
  - `Black` - A black leaf node.
  - `Branch MyTree MyTree MyTree MyTree` - A branch node with four subtrees.

## Key Functions

- **`allBlack`, `allWhite`**: Create uniform black or white trees.
- **`clockwise`, `anticlockwise`**: Create branches with subtrees in clockwise or anti-clockwise order.
- **`quadToList`**: Converts the tree to a list with node coordinates and colors.
- **`treeDepth`**: Calculates the depth of the tree.
- **`compareAdjacentNeighbours`**: Compares neighboring nodes based on color and position.
- **`transfer`**: Flips nodes based on adjacent color comparisons.
- **`putNode`**: Places a node at a specific position in the tree.
- **`makeQuad`**: Converts the list of nodes back into a quad tree.
- **`blur`**: Applies a blur effect by flipping nodes based on neighbors.

## Example Usage

```haskell
let tree = Branch Black White (Branch White Black Black White) White
let blurredTree = blur tree
